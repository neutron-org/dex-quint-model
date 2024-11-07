// -*- mode: Bluespec; -*-
module postconditions {

    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"
    import utils.* from "utils"
    import basicSpells.* from "lib/basicSpells"


    pure def withdrawPoolPrecondition(state: State, msg: WithdrawPoolMsg): bool = 
        true

    pure def withdrawPoolPostcondition(before: State, after: State, msg: WithdrawPoolMsg): bool = 
        val pair =  orderTokenPair(msg.tokenPair)
        val tick = if (pair == msg.tokenPair) msg.tick else -msg.tick
        val poolId = getPoolId(pair, tick, msg.fee)
        // We assume that we do not remove empty pools
        // We do not check that poolBefore exists, as it is a precondition and not a postcondition
        val reservesBefore = before.pools.get(poolId).reserves
        val reservesAfter = after.pools.get(poolId).reserves
        val sharesBefore = before.pools.get(poolId).shares
        val sharesAfter = after.pools.get(poolId).shares
        val outAmounts = computeAmountsWithdrawnPool(reservesBefore, sharesBefore, msg.sharesAmount)
        and{
            // ** State changes validation **
            // Reserves are updated
            reservesAfter._1 == reservesBefore._1 - outAmounts._1,
             reservesAfter._1 >= 0,
            reservesAfter._2 == reservesBefore._2 - outAmounts._2, 
            reservesAfter._2 >= 0,
            // Coins are transferred
            after.coins.get(msg.creator).get(pair._1) == before.coins.get(msg.creator).get(pair._1) + outAmounts._1,
            after.coins.get(msg.creator).get(pair._2) == before.coins.get(msg.creator).get(pair._2) + outAmounts._2,    
            // Shares are burned
            // We assume that we do not remove empty shares
            after.poolShares.get(msg.creator).get(poolId) >= 0,
            after.poolShares.get(msg.creator).get(poolId) == before.poolShares.get(msg.creator).get(poolId) - msg.sharesAmount,
            sharesAfter >= 0,
            sharesAfter == sharesBefore - msg.sharesAmount
            // ** Specification invariants **
        }



    pure def depositPostcondition(before: State, after: State, msg: DepositMsg): bool = 
        // We could guarantee by design that the token pair is submitted in order
        val pair =  orderTokenPair(msg.tokenPair)
        val amounts = if (pair == msg.tokenPair) msg.amountPair else (msg.amountPair._2, msg.amountPair._1)
        val token0SellTick = if (pair == msg.tokenPair) msg.token0SellTick else -msg.token0SellTick
        val poolId = getPoolId(pair, token0SellTick, msg.fee)
        // ** State changes validation **
        // Pool exists
        after.pools.has(poolId) and 
        val reservesBefore = if (before.pools.has(poolId)) before.pools.get(poolId).reserves else (0,0)
        val reservesAfter = after.pools.get(poolId).reserves
        val sharesBefore = if (before.pools.has(poolId)) before.pools.get(poolId).shares else 0
        val sharesAfter = after.pools.get(poolId).shares
        // Deposit and shares computation considering the optional autoswap
        val inAmountsBeforeAutoswap = computeAmountsDeposit(reservesBefore, amounts)
        val residualAmounts = (amounts._1 - inAmountsBeforeAutoswap._1, amounts._2 - inAmountsBeforeAutoswap._2)
        val inAmounts = if (msg.autoswap) amounts
                        else inAmountsBeforeAutoswap
        val autoswapFee = if (msg.autoswap) computeAutoswapFee(residualAmounts, token0SellTick, msg.fee) else zero
        val outShares = computeSharesDeposit(sharesBefore, reservesBefore, inAmounts, token0SellTick, autoswapFee)
        
        and{
            // Reserves are updated
            reservesAfter._1 == reservesBefore._1 + inAmounts._1,
            reservesAfter._2 == reservesBefore._2 + inAmounts._2,
            // Coins are transferred
            after.coins.get(msg.creator).get(pair._1) >= 0 and after.coins.get(msg.creator).get(pair._2) >= 0,
            after.coins.get(msg.creator).get(pair._1) == before.coins.get(msg.creator).get(pair._1) - inAmounts._1,
            after.coins.get(msg.creator).get(pair._2) == before.coins.get(msg.creator).get(pair._2) - inAmounts._2,
            // Shares are minted
            after.poolShares.get(msg.creator).has(poolId),
            after.poolShares.get(msg.creator).get(poolId) == 
            if (before.poolShares.get(msg.creator).has(poolId)) 
                before.poolShares.get(msg.creator).get(poolId) + outShares
            else 
                outShares,        
            sharesAfter == sharesBefore + outShares,
            // Pool value increases
            val poolValueBefore: Dec = 
                if (before.pools.has(poolId)) computePoolValue(before.pools.get(poolId).reserves, token0SellTick) else zero
            val poolValueAfter: Dec = computePoolValue(after.pools.get(poolId).reserves, token0SellTick)            
            
            /* Specification Invariants */
            // since this is a single step, setting the tolerance to 1
            poolValueAfter.approx_gt(poolValueBefore, one),
            
            // Pool ratio is maintained if no autoswap and both sides of the pool exist
            // @audit-issue DOES NOT HOLD
            // if (before.pools.has(poolId) and not(msg.autoswap) and before.pools.get(poolId).bothSides())
            //     val reservesBefore = before.pools.get(poolId).reserves
            //     val reservesAfter = after.pools.get(poolId).reserves
            //     if (reservesBefore._2 > 0) 
            //         val ratioBefore = fromRatio(reservesBefore._1, reservesBefore._2)
            //         val ratioAfter = fromRatio(reservesAfter._1, reservesAfter._2)
            //         approxEqualDec(ratioAfter, ratioBefore, div(ratioBefore, fromInt(5)))
            //     else reservesAfter._2 == 0
            // else true
        }

 pure def placeLimitOrderPostcondition(before: State, after: State, msg: PlaceLimitOrderMsg): bool =                
        val maxBuyTick = - msg.sellTick
        val fulfilledOrderInfo = swap(
            msg.tokenIn,
            msg.tokenOut,
            before.tranches,
            before.pools,
            msg.amountIn,
            maxBuyTick,
            msg.maxAmountOut
            
        )
        val poolsUpdateInfo = fulfilledOrderInfo.poolUpdates
        val tranchesUpdateInfo = debug("tranchesUpdates: ", fulfilledOrderInfo.tranchesUpdates)
        val remainingAmount = debug("remaining amounot = ", fulfilledOrderInfo.remainingAmount)
        val outAmount = debug("outAmount = ", fulfilledOrderInfo.outAmount)

        // A not fully filled fillOrKill: the state is not changed at all
        if (remainingAmount > 0 and msg.orderType == FillOrKill) 
            before == after
        else 
        and {
            // pools and tranches are updated correctly
            validatePoolsUpdates(before, after, poolsUpdateInfo),
            validateTranchesUpdates(before, after, tranchesUpdateInfo),
            swapInternallyConsistent(msg, fulfilledOrderInfo),
            
            // if there was still some extra amount, and we are not in the immediateOrCancel case, then it was added as a new tranche
            implies (
                remainingAmount > 0 and not(msg.orderType == ImmediateOrCancel),
                val userGotRest = if (msg.orderType == ImmediateOrCancel) outAmount else 0
                val userDiffOut = after.coins.get(msg.creator).get(msg.tokenOut) - before.coins.get(msg.creator).get(msg.tokenOut)
                val makerToken = msg.tokenIn
                val takerToken = msg.tokenOut
                val blockExpirationTime = match msg.orderType {
                    | GoodTillTime(time) => time
                    | GoodTillCancelled => -1
                    | JustInTime => before.blockNumber + 1
                    | _ => -1
                }
                
                val trancheKey = calculateTrancheKey(makerToken, takerToken, msg.sellTick, before.tranches, blockExpirationTime)                
                and{            
                    // user should have received the correct amount
                    outAmount + userGotRest == userDiffOut,
                    // the tranche key exists in the after state
                    after.tranches.keys().contains(trancheKey),
                    // the amount in this tranche is increased by the remainingAmount
                    val totalMakerAmountBefore = if (before.tranches.keys().contains(trancheKey)) before.tranches.get(trancheKey).totalMaker else 0
                    after.tranches.get(trancheKey).totalMaker == totalMakerAmountBefore + remainingAmount,
                    // the user's shares now exists
                    after.tranchesShares.get(msg.creator).keys().contains(trancheKey),
                    // the tranche shares are updated
                    val totalSharesBefore = 
                        if (before.tranches.keys().contains(trancheKey) and before.tranchesShares.get(msg.creator).keys().contains(trancheKey)) 
                            before.tranchesShares.get(msg.creator).get(trancheKey).sharesOwned 
                        else 0
                    val sharesDiff = after.tranchesShares.get(msg.creator).get(trancheKey).sharesOwned - totalSharesBefore
                    sharesDiff == remainingAmount                                                                    
                }
            ),

            // checking that the user's amount got updated correctly
            val tokenOutGained = outAmount            
            tokenOutGained == after.coins.get(msg.creator).get(msg.tokenOut) - before.coins.get(msg.creator).get(msg.tokenOut),            
            val tokenInSpent = tokenInSpentBasedOnOrderType(msg.orderType, msg.amountIn, remainingAmount)
            tokenInSpent == before.coins.get(msg.creator).get(msg.tokenIn) - after.coins.get(msg.creator).get(msg.tokenIn)
  
        } 

    pure def singlehopSwapPostcondition(before: State, after: State, msg: SwapMsg): bool =
        val inAmount = msg.amount
        val fulfilledOrderInfo = swap(
            msg.tokenPair._1,
            msg.tokenPair._2,            
            before.tranches,
            before.pools,
            inAmount,
            msg.limitPriceTick,
            0            
        )
        val poolsUpdateInfo = debug("pool update: ", fulfilledOrderInfo.poolUpdates)
        val tranchesUpdateInfo = fulfilledOrderInfo.tranchesUpdates
        val remainingAmount = debug("remaining Amount :",fulfilledOrderInfo.remainingAmount)
        val outAmount = debug("outAmounot: ", fulfilledOrderInfo.outAmount)
        val usedAmount = debug("usedAmount: ", inAmount - remainingAmount)

        
        and{
            // ** State changes validation **        
            // Coins are transferred
            after.coins.get(msg.creator).get(msg.tokenPair._1) == before.coins.get(msg.creator).get(msg.tokenPair._1) - usedAmount,
            after.coins.get(msg.creator).get(msg.tokenPair._2) == before.coins.get(msg.creator).get(msg.tokenPair._2) + outAmount,
            // Reserves are updated
            validatePoolsUpdates(before, after, poolsUpdateInfo),
            // Tranches changes
            validateTranchesUpdates(before, after, tranchesUpdateInfo),
            
            // ** Specification invariants **
            // Invariant Swap.1
            val buyingPrice = priceFromTick(msg.limitPriceTick)
            // the amount I would get at my limit price
            val minAmount = intDivDec(usedAmount, buyingPrice).truncate()
            outAmount >= minAmount,
            inAmount >= usedAmount,
            // // Invariant LP.3
            poolsUpdateInfo.keys().forall(id =>
                val nontrivialDiff = abs(poolsUpdateInfo.get(id).token0Diff) > 0 or abs(poolsUpdateInfo.get(id).token1Diff) > 0
                val poolValueBefore = computePoolValue(before.pools.get(id).reserves, id.tick)
                val poolValueAfter = computePoolValue(after.pools.get(id).reserves, id.tick)
                nontrivialDiff implies poolValueAfter.approx_gt(poolValueBefore, fromInt(3))
            )
        }

    pure def withdrawLimitOrderPostcondition(before: State, after: State, msg: WithdrawLimitOrderMsg): bool =
        val tranche = before.tranches.get(msg.trancheKey)
        val usersTrancheShares = before.tranchesShares.get(msg.creator).get(msg.trancheKey)
        val withdrawnAmounts = computeWithdrawAmountTakerToken(tranche, usersTrancheShares)
        val withdrawnAmountTakerToken = withdrawnAmounts.taker_token
        val withdrawnShares = withdrawnAmounts.shares
                
        and {
            // **State changes validation**
            // the user's taker balance has increased for what was withdrawn
            withdrawnAmountTakerToken == 
                after.coins.get(msg.creator).get(before.tranches.get(msg.trancheKey).takerDenomination) -
                before.coins.get(msg.creator).get(before.tranches.get(msg.trancheKey).takerDenomination),
            // the reserves has decreased for what was withdrawn
            withdrawnAmountTakerToken ==
                before.tranches.get(msg.trancheKey).reservesTaker -
                after.tranches.get(msg.trancheKey).reservesTaker,
            // the user's shares withdrawn have increased appropriately
            withdrawnShares == 
                after.tranchesShares.get(msg.creator).get(msg.trancheKey).sharesWithdrawn -
                before.tranchesShares.get(msg.creator).get(msg.trancheKey).sharesWithdrawn,

            // ** Invariants **

            // After a tranche withdrawal, the user will get the pro-rata portion of all
            // the swap proceeds not yet claimed (since the user’s last withdrawal).
            // [Maker Limit Order Invariants, 1]
            pure val swaps = before.bookkeeping.tranches.swaps.getOrElse((msg.creator, msg.trancheKey), Set())
            pure val userShares = before.tranchesShares.get(msg.creator).get(msg.trancheKey)
            pure val tranche = before.tranches.get(msg.trancheKey)
            pure def proRataShare(x) = ((userShares.sharesOwned * x) / tranche.totalMaker)
            pure val totalSwappedTakerAmount = swaps.fold(0, (acc, diff) => {
              acc + q::debug("diff", diff).takerReservesDiff
            })

            // there can be up to one off-by-one error per swap
            (withdrawnAmountTakerToken - proRataShare(totalSwappedTakerAmount)).in((-swaps.size()).to(swaps.size()))
    }

    pure def cancelLimitOrderPostcondition(before: State, after: State, msg: CancelLimitOrderMsg): bool =
        val tranche_before: Tranche = before.tranches.get(msg.trancheKey)
        val users_tranches_shares_before = before.tranchesShares.get(msg.creator).get(msg.trancheKey)
        val users_tranches_before = before.tranchesShares.get(msg.creator).keys()
        and {  
            // user received their fair amount of maker tokens back                        
            val cancelAmountOutMakerToken = computeCancelAmountMakerToken(tranche_before, users_tranches_shares_before)                                        
            // the user's balance has increased for what was cancelled                
            cancelAmountOutMakerToken == 
            after.coins.get(msg.creator).get(tranche_before.makerDenomination) -
            before.coins.get(msg.creator).get(tranche_before.makerDenomination)
            and
            // the totalMaker has decreased for what was cancelled                
            users_tranches_shares_before.sharesOwned ==
            before.tranches.get(msg.trancheKey).totalMaker -
            after.tranches.get(msg.trancheKey).totalMaker,
            
            // This part corresponds to the point 2b from the spec document
            val withdrawAmountTakerToken = computeWithdrawAmountTakerToken(tranche_before, users_tranches_shares_before).taker_token            
            // the user's taker token balance has increased for what was withdrawn when cancelling
            withdrawAmountTakerToken == 
                after.coins.get(msg.creator).get(tranche_before.takerDenomination) -
                before.coins.get(msg.creator).get(tranche_before.takerDenomination)
            and
            // the totalTaker has decreased for what was cancelled
            withdrawAmountTakerToken ==
                before.tranches.get(msg.trancheKey).totalTaker -
                after.tranches.get(msg.trancheKey).totalTaker,
                
            // ** Invariants **                        
        }

    /// After each singlehopSwap, deposit, or placeLimitOrder, the values of
    /// pools with the relevant pair has remained the same or it has increased.
    pure def nonDecreasingPools(before: State, after: State, tokenPair: TokenPair): bool = {
        pure def relevantPoolIdsBefore = before.pools.keys().filter(p => p.tokenPair == tokenPair)
        pure def relevantPoolIdsAfter = after.pools.keys().filter(p => p.tokenPair == tokenPair)

        and {
            relevantPoolIdsBefore.subseteq(relevantPoolIdsAfter),
            relevantPoolIdsBefore.forall(p => {
                approx_gte(
                    computePoolValue(after.pools.get(p).reserves, p.tick),
                    computePoolValue(before.pools.get(p).reserves, p.tick),
                    one
                )
            })
        }
    }
}
