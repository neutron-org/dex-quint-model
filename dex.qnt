// -*- mode: Bluespec; -*-

/**
 * The core of the Neutron DEX.
 */
module Dex {
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"
    import utils.* from "utils"
    import basicSpells.* from "lib/basicSpells"
    


    pure def withdrawPool(state: State, msg: WithdrawPoolMsg) : Result = {
      // deposits and poolWithdraws variable management
      val pair =  orderTokenPair(msg.tokenPair)
      val tick = if (pair == msg.tokenPair) msg.tick else -msg.tick
      val poolId = getPoolId(pair, tick, msg.fee)
      // pool must exist
      val pool = state.pools.get(poolId)
      val outAmounts = computeAmountsWithdrawPool(pool.reserves, pool.shares, msg.amount)
      // poolWithdraws variable management
      val withdrawns = state.poolWithdraws.get(msg.creator).get(poolId)
      val updatedWithdraws = (withdrawns._1 + outAmounts._1, withdrawns._2 + outAmounts._2)
      ok({poolWithdraws: state.poolWithdraws.set(msg.creator, state.poolWithdraws.get(msg.creator).set(poolId, updatedWithdraws)),
          ...state})                  
      //ok(state)
    }


    
  
    pure def deposit(state: State, msg: DepositMsg) : Result = {
        val pair =  orderTokenPair(msg.tokenPair)
        val amounts = if (pair == msg.tokenPair) msg.amountPair else (msg.amountPair._2, msg.amountPair._1)
        val tick0To1 = if (pair == msg.tokenPair) msg.tick else -msg.tick
        val poolId = getPoolId(pair, tick0To1, msg.fee)
        val pool = if (state.pools.has(poolId)) state.pools.get(poolId) else emptyPool
        val inAmountsBeforeAutoswap = computeAmountsDeposit(pool.reserves, amounts)
        val residualAmounts = (amounts._1 - inAmountsBeforeAutoswap._1, amounts._2 - inAmountsBeforeAutoswap._2)
        val performAutoswap: bool = msg.autoswap and (residualAmounts._1 > 0 or residualAmounts._2 > 0)
        val inAmounts = if (performAutoswap) amounts else inAmountsBeforeAutoswap
        val autoswapFee = if (performAutoswap) computeAutoswapFee(residualAmounts, tick0To1, msg.fee) else zero
        val outShares = computeSharesDeposit(pool.shares, pool.reserves, inAmounts, tick0To1, autoswapFee)

        // updating the pool shares
        val initShares = state.poolShares.getOrElse(msg.creator, Map()).getOrElse(poolId, 0)
        val updatedShares = state.poolShares.put(
            msg.creator,
            state.poolShares.getOrElse(msg.creator, Map()).put(poolId, initShares + outShares)
        )

        // updating the pools        
        val updatedPools = state.pools.put(
            poolId,
            {
                reserves: (pool.reserves._1 + inAmounts._1, pool.reserves._2 + inAmounts._2), 
                shares: pool.shares + outShares
            }

        )

        // deposits and poolWithdraws variable management
        val preExistingDeposit = 
            state.deposits.get(msg.creator).has(poolId) and state.poolShares.get(msg.creator).get(poolId) > 0                        
        val deposit = if (preExistingDeposit) state.deposits.get(msg.creator).get(poolId)
                      else {amounts: (0,0), swaps: false}
        
        val updatedDeposit = {
            amounts: (deposit.amounts._1 + inAmounts._1, deposit.amounts._2 + inAmounts._2),
            swaps: if (preExistingDeposit) deposit.swaps else false
            }
        
        val updatedCreatorCoins = state.coins.get(msg.creator).setBy(
            pair._1, old => old - inAmounts._1
        ).setBy(
            pair._2, old => old - inAmounts._2
        )

        val updatedState = {
            deposits: state.deposits.setBy(msg.creator, old => old.put(poolId, updatedDeposit)),            
            poolShares: updatedShares,
            pools: updatedPools,
            coins: state.coins.set(msg.creator, updatedCreatorCoins),
            ...state
            }
            
        ok(updatedState)                  
    }


    
    
    
    pure def placeLimitOrder(state: State, msg: PlaceLimitOrderMsg) : Result =        
        val fulfilledOrderInfo = swap(
            msg.tokenIn, 
            msg.tokenOut,            
            state.tranches,
            state.pools,
            msg.amountIn,
            msg.tick,
            msg.maxAmountOut            
        )
        
        val poolsUpdateInfo = fulfilledOrderInfo.poolUpdates
        val tranchesUpdateInfo = fulfilledOrderInfo.tranchesUpdates
        val remainingAmount = fulfilledOrderInfo.remainingAmount
        val tokenOutReceived = fulfilledOrderInfo.outAmount

        if (remainingAmount > 0 and msg.orderType == FillOrKill)
            {state: state, error: "FillOrKill: not enough liquidity"}
        else
            // 1) update the pools states
            val updatedPools = updatePools(state.pools, poolsUpdateInfo)
            
            // 2) update the tranches states
            val tradeUpdatedTranches: TrancheKey->Tranche = updateTranches(state.tranches, tranchesUpdateInfo)
            // 2a) handle the remaining amount and update tranches accordingly
            val tranchesSharesUpdate = 
                // no new updates to tranches if the type is ImmediateOrCancel
                if (msg.orderType == ImmediateOrCancel) 
                    {tranches: tradeUpdatedTranches, tranchesShares: state.tranchesShares}
                else 
                    val expirationBlock = match msg.orderType {
                        | GoodTillTime(block_time) => block_time
                        | JustInTime => state.blockNumber + 1
                        | GoodTillCancelled => -1 
                        | _ => -2 // unreachable
                    }
                    placeRemainingLimitOrderAmount(tradeUpdatedTranches, state.tranchesShares, msg, remainingAmount, expirationBlock)

            
            val tokenInSpent = tokenInSpentBasedOnOrderType(msg.orderType, msg.amountIn, remainingAmount)
            // 3) update the user's balance
            val updatedUserCoins = state.coins.get(msg.creator).keys().mapBy(
                token => 
                if (token == msg.tokenOut)
                    state.coins.get(msg.creator).get(token) + tokenOutReceived
                else if (token == msg.tokenIn)
                    state.coins.get(msg.creator).get(token) - tokenInSpent
                else
                    state.coins.get(msg.creator).get(token)                
            )


            val new_state = {
                tranches: tranchesSharesUpdate.tranches,
                tranchesShares: tranchesSharesUpdate.tranchesShares,
                pools: updatedPools,
                coins: state.coins.set(msg.creator, updatedUserCoins),
                ...state
            }
            ok(new_state)
    
    pure def singlehopSwap(state: State, msg: SwapMsg) : Result = {
      val inAmount = msg.amount
      val fulfilledOrderInfo = swap(
            msg.tokenPair._1,
            msg.tokenPair._2,            
            state.tranches,
            state.pools,
            inAmount,
            msg.limitPriceTick,
            0            
        )
        val poolsUpdateInfo = fulfilledOrderInfo.poolUpdates
        val tranchesUpdateInfo = fulfilledOrderInfo.tranchesUpdates
        val remainingAmount = fulfilledOrderInfo.remainingAmount
        val outAmount = fulfilledOrderInfo.outAmount
        val usedAmount = inAmount - remainingAmount

        // 1) update the coins of the user
        val updatedCoins = state.coins.get(msg.creator)
                .setBy(msg.tokenPair._1, old => old - usedAmount)
                .setBy(msg.tokenPair._2, old => old + outAmount)
        // 2) update the reserves of the pools
        val updatedPools = updatePools(state.pools, poolsUpdateInfo)

        // 3) update the tranches
        val updatedTranches = updateTranches(state.tranches, tranchesUpdateInfo)

        // 4) update the user's deposits (helper variable)
        val updatedDeposits = CREATORS.fold(
        state.deposits, 
        (acc, user) => 
            acc.set(user, updateSwapsBool(msg.creator, poolsUpdateInfo.keys(),state.deposits.get(user),state.poolShares.get(user)))
        )                                                

        val updatedState = {
            coins: state.coins.set(msg.creator, updatedCoins),
            pools: updatedPools,
            tranches: updatedTranches,
            deposits: updatedDeposits,
            ...state
        }

        ok(updatedState)
    }

    pure def withdrawLimitOrder(state: State, msg: WithdrawLimitOrderMsg) : Result =
        val tranche = state.tranches.get(msg.trancheKey)
        val usersTrancheShares = state.tranchesShares.get(msg.creator).get(msg.trancheKey)        
        val withdrawnAmounts = computeWithdrawAmountTakerToken(tranche, usersTrancheShares)
        val withdrawnTaker = withdrawnAmounts.taker_token
        val withdrawnShares = withdrawnAmounts.shares
        
        // 1) user's taker token balance needs to be updated
        val updatedUsersCoins = state.coins.get(msg.creator).get(tranche.takerDenomination) + withdrawnTaker
        // 2) the reserves taker denom need to be updated
        val updatedTrancheTakerReserves = tranche.reservesTaker - withdrawnTaker
        // 3) updating the user's shares: increasing the withdrawn shares
        val updatedUserShares = {sharesWithdrawn: usersTrancheShares.sharesWithdrawn + withdrawnShares, ...usersTrancheShares}
        val updatedState = 
        {
            coins: state.coins.setBy(
                msg.creator, 
                old => old.set(tranche.takerDenomination, updatedUsersCoins)),
            tranches: state.tranches.set(msg.trancheKey, {reservesTaker: updatedTrancheTakerReserves, ...tranche}),
            tranchesShares: state.tranchesShares.setBy(
                msg.creator, 
                old => old.set(msg.trancheKey, updatedUserShares)),
            ...state
        }
        ok(updatedState)


    pure def cancelLimitOrder(state: State, msg: CancelLimitOrderMsg) : Result =
        // 1) update the user's tranchesShares record
        val updatedUsersTranches = 
            state.tranchesShares.get(msg.creator).keys().fold(
                Map(),
                (acc, trancheKey) => 
                if (trancheKey == msg.trancheKey)
                    acc.put(trancheKey, {sharesOwned: 0, sharesWithdrawn: 0})
                else
                    acc.put(trancheKey, state.tranchesShares.get(msg.creator).get(trancheKey))
            )

        val tranche = state.tranches.get(msg.trancheKey)
        val usersTrancheShares = state.tranchesShares.get(msg.creator).get(msg.trancheKey)
        // 2) update the user's coins
        // 2a) calculate the amount of the tranche maker token that the user should receive back                                
        val cancelAmountMakerToken = computeCancelAmountMakerToken(tranche, usersTrancheShares)
        val updatedUserCoinsMaker = state.coins.get(msg.creator).get(msg.trancheKey.makerToken) + cancelAmountMakerToken
        
        // 2b) calculate the amount of the tranche taker token that the user should receive back (withdraw equivalent)
        val withdrawAmountTakerToken = computeWithdrawAmountTakerToken(tranche, usersTrancheShares).taker_token            
        val updatedUserCoinsTaker = state.coins.get(msg.creator).get(msg.trancheKey.takerToken) + withdrawAmountTakerToken
       
        // 3) update the tranche reserves
        val updatedTranche = {
            reservesMaker: tranche.reservesMaker - cancelAmountMakerToken,
            totalMaker: tranche.totalMaker - state.tranchesShares.get(msg.creator).get(msg.trancheKey).sharesOwned,
            // @audit-info This is not mentioned in the spec, but it seems necessary
            reservesTaker: tranche.reservesTaker - withdrawAmountTakerToken,
            totalTaker: tranche.totalTaker - withdrawAmountTakerToken,
            ...tranche
        }
        

        // updated state
        val updatedState =
        {
            tranchesShares: state.tranchesShares.set(msg.creator, updatedUsersTranches),
            coins: state.coins.set(
                msg.creator,
                state.coins.get(msg.creator)
                    .set(msg.trancheKey.makerToken, updatedUserCoinsMaker)
                    .set(msg.trancheKey.takerToken, updatedUserCoinsTaker)
                ),
            tranches: state.tranches.set(msg.trancheKey, updatedTranche),
            ... state
        }
        ok(updatedState)
    

    pure def advanceBlocks(state: State, numBlocks: int) : Result = {
        val newBlock = state.blockNumber + numBlocks
        val updatedTranches = state.tranches.keys().mapBy(
            k =>
                val tranche = state.tranches.get(k) 
                if (tranche.mayExpire() and tranche.expirationTime < newBlock)
                    {isActive: false, ...state.tranches.get(k)}
                  else
                    state.tranches.get(k)
        )
        
        val updatedState = {blockNumber: newBlock, tranches: updatedTranches, ...state}
        ok(updatedState)
    }

}
