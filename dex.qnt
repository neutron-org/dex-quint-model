// -*- mode: Bluespec; -*-

/**
 * The core of the Neutron DEX.
 * 
 *
 * 
 *
 *
 */
module Dex {
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"
    import utils.* from "utils"
    import basicSpells.* from "basicSpells"
    
    // @audit-info When to truncate as a general rule?

    // CH: STATE VARIABLES
    var state: State  
    var postconditionOK: bool
    var actionSuccess: bool
    var actionTaken: Transition

    // CH: STATE MACHINE
    action step: bool = any {
        placeLimitOrderAction,
        cancelLimitOrderAction,
        withdrawLimitOrderAction,
        advanceTimeAction
    }

    action init = all {
        state' = initState,
        postconditionOK' = true,
        actionTaken' = {name: "init", message: Init}
    }

    // action specialAction = 
    //     val alicesOrder: PlaceLimitOrderMsg = {
    //             creator: "Alice",
    //             orderType: GoodTillCancelled,
    //             tokenIn: "B",
    //             tokenOut: "A",
    //             amountIn: 20,
    //             tick: 60,
    //             maxAmountOut: -1
    //         }
    //         all{
    //             state' = placeLimitOrder(state, alicesOrder),

        

    // ACTIONS

    //    CH: WITHDRAWING FROM POOLS
    
    
    
      // ** Specification invariants **

    pure def withdrawPool(state: State, msg: WithdrawPoolMsg) : Result = {
      // deposits and poolWithdrawns variable management
      val pair =  orderTokenPair(msg.tokenPair)
      val tick = if (pair == msg.tokenPair) msg.tick else -msg.tick
      val poolId = getPoolId(pair, tick, msg.fee)
      // pool must exist
      val pool = state.pools.get(poolId)
      val outAmounts = computeAmountsWithdrawPool(pool.reserves, pool.shares, msg.amount)
      // poolWithdrawns variable management
      val withdrawns = state.poolWithdrawns.get(msg.creator).get(poolId)
      val updatedWithdraws = (withdrawns._1 + outAmounts._1, withdrawns._2 + outAmounts._2)
      ok({poolWithdrawns: state.poolWithdrawns.set(msg.creator, state.poolWithdrawns.get(msg.creator).set(poolId, updatedWithdraws)),
          ...state})                  
      //ok(state)
    }

    pure def withdrawPoolPrecondition(state: State, msg: WithdrawPoolMsg): bool = 
        true

    action withdrawAction(msg: WithdrawPoolMsg): bool = {
      val result = withdrawPool(state, msg)
      all {
        withdrawPoolPrecondition(state, msg),
        require(result.error=="nil"),
        state' = result.state,
        postconditionOK' = withdrawPoolPostcondition(state, result.state, msg),
        actionTaken' = {name: "withdraw", message: WithdrawPoolSubType(msg)}
      }
    }

    
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
      val outAmounts = computeAmountsWithdrawPool(reservesBefore, sharesBefore, msg.amount)
      // ** State changes validation **
      // Reserves are updated
      reservesAfter._1 == reservesBefore._1 - outAmounts._1 and reservesAfter._1 >= 0 and
      reservesAfter._2 == reservesBefore._2 - outAmounts._2 and reservesAfter._2 >= 0 and
      // Coins are transferred
      after.coins.get(msg.creator).get(pair._1) == before.coins.get(msg.creator).get(pair._1) + outAmounts._1 and
      after.coins.get(msg.creator).get(pair._2) == before.coins.get(msg.creator).get(pair._2) + outAmounts._2 and
      // Shares are burned
      // We assume that we do not remove empty shares
      after.poolShares.get(msg.creator).get(poolId) >= 0 and
      after.poolShares.get(msg.creator).get(poolId) == before.poolShares.get(msg.creator).get(poolId) - msg.amount and
      sharesAfter >= 0 and sharesAfter == sharesBefore - msg.amount
      // ** Specification invariants **

    
  
    //    CH: DEPOSITING INTO POOLS
    pure def deposit(state: State, msg: DepositMsg) : Result = {
      val pair =  orderTokenPair(msg.tokenPair)
      val amounts = if (pair == msg.tokenPair) msg.amountPair else (msg.amountPair._2, msg.amountPair._1)
      val tick = if (pair == msg.tokenPair) msg.tick else -msg.tick
      val poolId = getPoolId(pair, tick, msg.fee)
      val pool = if (state.pools.has(poolId)) state.pools.get(poolId)
                 else {reserves: (0, 0), shares: 0}
      val inAmounts = computeAmountsDeposit(pool.reserves, amounts)
      // deposits and poolWithdrawns variable management
      val isNewPeriod = if (state.deposits.get(msg.creator).has(poolId) and state.poolShares.get(msg.creator).get(poolId) > 0) false 
                        else true
      val deposit = if (not(isNewPeriod)) state.deposits.get(msg.creator).get(poolId)
                    else {amounts: (0,0), swaps: false}
      val withdrawns = if (not(isNewPeriod)) state.poolWithdrawns.get(msg.creator).get(poolId)
                       else (0,0)
      val updatedDeposit = {amounts: (deposit.amounts._1 + inAmounts._1, deposit.amounts._2 + inAmounts._2),
                            swaps: if (isNewPeriod) false else deposit.swaps}
      ok({deposits: state.deposits.set(msg.creator, state.deposits.get(msg.creator).mapSafeSet(poolId, updatedDeposit)),
          poolWithdrawns: state.poolWithdrawns.set(msg.creator, state.poolWithdrawns.get(msg.creator).mapSafeSet(poolId, withdrawns)),
          ...state})                  
      //ok(state)
    }

    action depositAction(msg: DepositMsg): bool = {
      val result = deposit(state, msg)
      all {
        require(result.error=="nil"),
        state' = result.state,
        postconditionOK' = depositPostcondition(state, result.state, msg),
        actionTaken' = {name: "deposit", message: DepositPoolSubType(msg)}
      }
    }

     // Can totalPoolValue be 0? Invariant candidate!

    pure def depositPostcondition(before: State, after: State, msg: DepositMsg): bool = 
      // We could guarantee by design that the token pair is submitted in order
      val pair =  orderTokenPair(msg.tokenPair)
      val amounts = if (pair == msg.tokenPair) msg.amountPair else (msg.amountPair._2, msg.amountPair._1)
      val tick = if (pair == msg.tokenPair) msg.tick else -msg.tick
      val poolId = getPoolId(pair, tick, msg.fee)
      // ** State changes validation **
      // Pool exists
      after.pools.has(poolId) and 
      val reservesBefore = if (before.pools.has(poolId)) (0,0) else before.pools.get(poolId).reserves
      val reservesAfter = after.pools.get(poolId).reserves
      val sharesBefore = if (before.pools.has(poolId)) 0 else before.pools.get(poolId).shares
      val sharesAfter = after.pools.get(poolId).shares
      // Deposit and shares computation considering the optional autoswap
      val inAmountsBeforeAutoswap = computeAmountsDeposit(reservesBefore, amounts)
      val residualAmounts = (amounts._1 - inAmountsBeforeAutoswap._1, amounts._2 - inAmountsBeforeAutoswap._2)
      val inAmounts = if (msg.autoswap) amounts
                      else inAmountsBeforeAutoswap
      val autoswapFee = if (msg.autoswap) computeAutoswapFee(residualAmounts, tick) else zero
      val outShares = computeSharesDeposit(sharesBefore, reservesBefore, inAmounts, tick, autoswapFee)
      // Reserves are updated
      reservesAfter._1 == reservesBefore._1 + inAmounts._1 and 
      reservesAfter._2 == reservesBefore._2 + inAmounts._2 and
      // Coins are transferred
      after.coins.get(msg.creator).get(pair._1) >= 0 and after.coins.get(msg.creator).get(pair._2) >= 0 and
      after.coins.get(msg.creator).get(pair._1) == before.coins.get(msg.creator).get(pair._1) - inAmounts._1 and
      after.coins.get(msg.creator).get(pair._2) == before.coins.get(msg.creator).get(pair._2) - inAmounts._2 and
      // Shares are minted
      after.poolShares.get(msg.creator).has(poolId) and
      after.poolShares.get(msg.creator).get(poolId) == 
      (if (before.poolShares.get(msg.creator).has(poolId)) 
            before.poolShares.get(msg.creator).get(poolId) + outShares
        else 
            outShares) 
      and
      sharesAfter == sharesBefore + outShares and
      // Pool value increases
      val poolValueBefore = if (before.pools.has(poolId)) computePoolValue(before.pools.get(poolId).reserves, tick) else 0
      val poolValueAfter = computePoolValue(after.pools.get(poolId).reserves, tick)
      poolValueAfter > poolValueBefore


    
    
    
    //    CH: PLACING A LIMIT ORDER
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
        val userReceived = fulfilledOrderInfo.outAmount

        // 1) update the pools states
        val updatedPools = state.pools.keys().mapBy(poolId =>
            if (poolsUpdateInfo.keys().contains(poolId))
                val updatedReserves = (state.pools.get(poolId).reserves._1 + poolsUpdateInfo.get(poolId).token0Diff,
                                      state.pools.get(poolId).reserves._2 + poolsUpdateInfo.get(poolId).token1Diff)
                {
                    reserves: updatedReserves,
                    shares: state.pools.get(poolId).shares
                }
            else
                state.pools.get(poolId)            
        )
        

        // 2) update the tranches states
        val tradeUpdatedTranches = state.tranches.keys().mapBy(
            trancheKey =>
            if (tranchesUpdateInfo.keys().contains(trancheKey))
                {
                    reservesMaker: state.tranches.get(trancheKey).reservesMaker + tranchesUpdateInfo.get(trancheKey).makerReservesDiff,
                    reservesTaker: state.tranches.get(trancheKey).reservesTaker + tranchesUpdateInfo.get(trancheKey).takerReservesDiff,
                    isFresh: false,
                    ...state.tranches.get(trancheKey),
                }
            else
                state.tranches.get(trancheKey)
        )

        // 3) update the user's balance
        val updatedUserCoins = state.coins.get(msg.creator).keys().mapBy(
            token => 
            if (token == msg.tokenOut)
                state.coins.get(msg.creator).get(token) + userReceived
            else if (token == msg.tokenIn)
                state.coins.get(msg.creator).get(token) - (msg.amountIn - remainingAmount)
            else
                state.coins.get(msg.creator).get(token)
                
        )


        val tradeUpdatedState = {
            tranches: tradeUpdatedTranches,
            pools: updatedPools,
            coins: state.coins.set(msg.creator, updatedUserCoins),
            ...state
        }
        // Different message types handle remaining amount differently:
        match msg.orderType {
            // GoodTillCancelled orders add it to liquidity
            | GoodTillCancelled =>
                val newTrancheUpdatedState = handleRemainingLimitOrderAmount(tradeUpdatedState, msg, remainingAmount, -1)
                ok(newTrancheUpdatedState)

            // GoodTillTime orders add it to liquidity, but with the specified expiration time
            | GoodTillTime(block_time) =>
                val newTrancheUpdatedState = handleRemainingLimitOrderAmount(tradeUpdatedState, msg, remainingAmount, block_time)
                ok(newTrancheUpdatedState)
            
            // JustInTime order add it to liquidity, but with the next block as expiration time
            | JustInTime =>
                val newTrancheUpdatedState = handleRemainingLimitOrderAmount(tradeUpdatedState, msg, remainingAmount, state.blockNumber + 1)
                ok(newTrancheUpdatedState)


            // FillOrKill orders fail if there is remaining amount
            | FillOrKill =>
                if (remainingAmount > 0)
                    {state: tradeUpdatedState, error: "FillOrKill: not enough liquidity"}
                else
                    ok(tradeUpdatedState)

            // ImmediateOrCancel orders return the remaining amount to the user
            | ImmediateOrCancel =>
                val stateRemainingAmountReturned = {
                    coins: tradeUpdatedState.coins.setBy(
                        msg.creator, 
                        old => old.set(msg.tokenIn, old.get(msg.tokenIn) + remainingAmount)),
                    ...tradeUpdatedState,
                }
                ok(tradeUpdatedState)
            | _ => ok(tradeUpdatedState)
        }


        

        
        
    
    pure def validatePoolsUpdates(before: State, after: State, poolsUpdates: PoolId -> PoolUpdateDiff) : bool = 
        // updated pools equal the 'after' state, the rest is unchanged
        before.pools.keys().forall(poolId => 
            if (poolsUpdates.keys().contains(poolId))
                val updatedReserves = (before.pools.get(poolId).reserves._1 + poolsUpdates.get(poolId).token0Diff,
                                        before.pools.get(poolId).reserves._2 + poolsUpdates.get(poolId).token1Diff)
                after.pools.get(poolId).reserves == updatedReserves
            else
                before.pools.get(poolId).reserves == after.pools.get(poolId).reserves
        )

    pure def validateTrachesUpdates(before: State, after: State, tranchesUpdateInfo: TrancheKey -> TrancheUpdateDiff) : bool = 
        // updated tranches equal the 'after' state, the rest is unchanged
            before.tranches.keys().forall(trancheKey =>
                if (tranchesUpdateInfo.keys().contains(trancheKey))
                    before.tranches.get(trancheKey) ==
                    {
                        reservesMaker: before.tranches.get(trancheKey).reservesMaker + tranchesUpdateInfo.get(trancheKey).makerReservesDiff,
                        reservesTaker: before.tranches.get(trancheKey).reservesTaker + tranchesUpdateInfo.get(trancheKey).takerReservesDiff,
                        ...before.tranches.get(trancheKey),
                    }
                else
                    before.tranches.get(trancheKey) == after.tranches.get(trancheKey)
            )
    
    pure def placeLimitOrderPostcondition(before: State, msg: PlaceLimitOrderMsg, after: State) : bool =
        // TODO: incorporate also here different types of orders (as it was done in the action)
        val fulfilledOrderInfo = swap(
            msg.tokenIn,
            msg.tokenOut,
            before.tranches,
            before.pools,
            msg.amountIn,
            msg.tick,
            msg.maxAmountOut
            
        )
        val poolsUpdateInfo = fulfilledOrderInfo.poolUpdates
        val tranchesUpdateInfo = fulfilledOrderInfo.tranchesUpdates
        val remainingAmount = fulfilledOrderInfo.remainingAmount
        val outAmount = fulfilledOrderInfo.outAmount

        
        and{
            validatePoolsUpdates(before, after, poolsUpdateInfo),
            validateTrachesUpdates(before, after, tranchesUpdateInfo),

            // summing up the total amount that the user received and checking if that is what the user has now
            val isPairNormalized = tokenLT(msg.tokenIn, msg.tokenOut, TOKEN_NUM_VALUES)
            val userGotFromPools = poolsUpdateInfo.keys().fold(
                0,
                (acc, poolId) => 
                val userGot = 
                    if (isPairNormalized) 
                        abs(poolsUpdateInfo.get(poolId).token1Diff)
                    else
                        abs(poolsUpdateInfo.get(poolId).token0Diff)                
                acc + userGot
            )
            val userGotFromTranches = tranchesUpdateInfo.keys().fold(
                0,
                (acc, trancheKey) => 
                // the abs is because of the way the diff is expressed as negative to signify that the amount was taken from the tranche
                val userGot = abs(tranchesUpdateInfo.get(trancheKey).makerReservesDiff)
                
                acc + userGot
            )
            val userDiffOut = after.coins.get(msg.creator).get(msg.tokenOut) - before.coins.get(msg.creator).get(msg.tokenOut)
            // user should have received the correct amount
            outAmount == userDiffOut,

            // if there was still some extra amount, it was added to limit orders
            implies(            
                remainingAmount > 0,
                val makerToken = msg.tokenIn
                val takerToken = msg.tokenOut
                val trancheIndex = calculateTrancheIndex(makerToken, takerToken, msg.tick, before.tranches.keys())
                val trancheKey = {
                    makerToken: makerToken,
                    takerToken: takerToken,
                    tick: msg.tick,
                    id: trancheIndex
                }

                and{
                    // the tranche key exists in the after state
                    after.tranches.keys().contains(trancheKey),

                // the amount in this tranched is increased by the remainingAmount
                val totalMakerAmountBefore = if (before.tranches.keys().contains(trancheKey)) before.tranches.get(trancheKey).totalMaker else 0
                after.tranches.get(trancheKey).totalMaker == totalMakerAmountBefore + remainingAmount,

                // the user's shares now exists
                after.tranchesShares.get(msg.creator).keys().contains(trancheKey),

                // the tranche shares are updated
                val totalSharesBefore = 
                    if (before.tranches.keys().contains(trancheKey) and before.tranchesShares.get(msg.creator).keys().contains(trancheKey)) 
                        before.tranchesShares.get(msg.creator).get(trancheKey).sharesOwned 
                    else 0
                val sharesDiff = after.tranchesShares.get(msg.creator).get(trancheKey).sharesOwned - 
                                 totalSharesBefore
                sharesDiff == remainingAmount                                                                    

                }
            ) 

                

            

        }      
        
    
        
        


        


    
    

        
      
        
        

    action placeLimitOrderAction: bool =
        nondet c = oneOf(CREATORS)
        nondet o_init = oneOf(ORDER_TYPES)
        nondet block_delta = oneOf(1.to(MAX_FUTURE_BLOCKS))
        val o = if (o_init == GoodTillTime(-1)) GoodTillTime(state.blockNumber + block_delta) else o_init

        nondet t = oneOf(TICKS)
        nondet tokenIn = oneOf(TOKENS)
        nondet tokenOut = oneOf(TOKENS.filter(t => t != tokenIn)) 
        // @audit-info
        // with this choice, we are not modelling user's funds. The assumption is that the user has as much as needed
        // And can replenish the funds as needed.
        // This implies that the only way to fill increase modeled coins is by withdrawing from the DEX.
        // This is a simplification that we can revisit later.
        nondet amount = oneOf(1.to(MAX_AMOUNT))
        // not implemented for now, so using a trivial value
        val maxAmountOut = amount + 1
        val limitOrderMsg = {
            creator: c,
            orderType: o,
            tick: t,
            tokenIn: tokenIn,
            tokenOut: tokenOut,
            amountIn: amount,
            maxAmountOut: maxAmountOut
        }
        val res = placeLimitOrder(state, limitOrderMsg)

        all {
            state' = res.state,
            // @audit-info TODO: this check is until Postcondition is updated to support different types
            postconditionOK' = 
            if (limitOrderMsg.orderType == GoodTillCancelled ) 
                placeLimitOrderPostcondition(state, limitOrderMsg, res.state)
            else true,
            actionTaken' = {name: "placeLimitOrder", message: PlaceLimitOrderSubType(limitOrderMsg)},
            actionSuccess' = res.error == "nil"
        }   

    
    
    
    //    CH: SWAPPING

    action singlehopSwapAction(msg: SwapMsg): bool = {
      val result = singlehopSwap(state, msg)
      all {
        require(result.error=="nil"),
        state' = result.state,
        postconditionOK' = singlehopSwapPostcondition(state, result.state, msg),
        actionTaken' = {name: "swap", message: SwapSubType(msg)}
      }
    }
    
    pure def singlehopSwap(state: State, msg: SwapMsg) : Result = {
      val inAmount = msg.amount
      val pair =  orderTokenPair(msg.tokenPair)
      val resultIteration = poolsLiquidityIteration(pair, msg.tokenPair._1, state.pools, inAmount)
      // deposits variable management
      val relevantPoolIds = resultIteration.changes.keys().filter(id => val changes = resultIteration.changes.get(id)
                                                                        changes._1 + changes._2 > 0)
      val updatedDeposits = CREATORS.fold(state.deposits, (acc, user) => acc.set(user, updateSwapsBool(msg.creator,
                                                                                                       relevantPoolIds,
                                                                                                       state.deposits.get(user),
                                                                                                       state.poolShares.get(user))))
      ok({deposits: updatedDeposits, ...state})
      //ok(state)
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
        val poolsUpdateInfo = fulfilledOrderInfo.poolUpdates
        val tranchesUpdateInfo = fulfilledOrderInfo.tranchesUpdates
        val remainingAmount = fulfilledOrderInfo.remainingAmount
        val outAmount = fulfilledOrderInfo.outAmount
        val usedAmount = inAmount - remainingAmount

      // ** State changes validation **
      
      and{
        // Coins are transferred
        after.coins.get(msg.creator).get(msg.tokenPair._1) == before.coins.get(msg.creator).get(msg.tokenPair._1) - usedAmount,
        after.coins.get(msg.creator).get(msg.tokenPair._2) == before.coins.get(msg.creator).get(msg.tokenPair._2) + outAmount,
        // Reserves are updated
        validatePoolsUpdates(before, after, poolsUpdateInfo),
        // Tranches changes
        validateTrachesUpdates(before, after, tranchesUpdateInfo),
        // ** Specification invariants **
        // Invariant Swap.1
        val exchangeRate = priceFromTick(msg.limitPriceTick)
        val minAmount = usedAmount.fromInt().mul(exchangeRate).truncate()
        outAmount >= minAmount,
        inAmount >= usedAmount,
        // Invariant LP.3
        poolsUpdateInfo.keys().forall(id =>
            val poolValueBefore = computePoolValue(before.pools.get(id).reserves, id.tick)
            val poolValueAfter = computePoolValue(after.pools.get(id).reserves, id.tick)
            poolValueAfter > poolValueBefore
        )
      }
      
    
 
    
    //   CH: WITHDRAWING PROCEEDS OF A TRANCHE

    pure def withdrawLimitOrder(state: State, msg: WithdrawLimitOrderMsg) : Result =
        val tranche = state.tranches.get(msg.trancheKey)
        val userTrancheShares = state.tranchesShares.get(msg.creator).get(msg.trancheKey)
        val withdrawnShares: int =
            calculatePercentFilled(tranche).mulInt(userTrancheShares.sharesOwned).truncate() - userTrancheShares.sharesWithdrawn
        val priceTakerToMaker = priceFromTick(tranche.tick)
        val withdrawnAmountTakerToken: int = intDivDec(withdrawnShares, priceTakerToMaker).truncate()
        
        // 1) user's balance needs to be updated
        val updatedUsersCoins = state.coins.get(msg.creator).get(tranche.takerDenomination) + withdrawnAmountTakerToken
        // 2) the reserves taker denom need to be updated
        val updatedTrancheTakerReserves = tranche.reservesTaker - withdrawnAmountTakerToken
        // 3) updating the user's shares: increasing the withdrawn shares
        val updatedUserShares = {sharesWithdrawn: userTrancheShares.sharesWithdrawn + withdrawnShares, ...userTrancheShares}
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
    
    pure def withdrawLimitOrderPostcondition(before: State, msg: WithdrawLimitOrderMsg, after: State) : bool = 
        val withdrawnShares: int = computeWithdrawTrancheShares(
            calculatePercentFilled(before.tranches.get(msg.trancheKey)),
            calculateUserNonWithdrawnTrancheShare(before.tranches.get(msg.trancheKey), before.tranchesShares.get(msg.creator))
        )
        val withdrawnAmountTakerToken: int = computeWithdrawTrancheAmountTakerToken(
            calculatePercentFilled(before.tranches.get(msg.trancheKey)),
            calculateUserNonWithdrawnTrancheShare(before.tranches.get(msg.trancheKey), before.tranchesShares.get(msg.creator)),
            before.tranches.get(msg.trancheKey).totalTaker
        )
        and {
            // the user's taker balance has increased for what was withdrawn
            withdrawnAmountTakerToken == 
                after.coins.get(msg.creator).get(before.tranches.get(msg.trancheKey).takerDenomination) -
                before.coins.get(msg.creator).get(before.tranches.get(msg.trancheKey).takerDenomination),
            // the totalTaker has decreased for what was withdrawn
            withdrawnAmountTakerToken ==
                before.tranches.get(msg.trancheKey).totalTaker -
                after.tranches.get(msg.trancheKey).totalTaker,
            // the user's shares withdrawn have increased appropriately
            withdrawnShares == 
                after.tranchesShares.get(msg.creator).get(msg.trancheKey).sharesWithdrawn -
                before.tranchesShares.get(msg.creator).get(msg.trancheKey).sharesWithdrawn
            
        }

    


    
action withdrawLimitOrderAction: bool = 
        nondet creator = oneOf(state.tranchesShares.keys())
        // TODO: add a restriction that it can only apply to "real" existing tranches
        nondet trancheKey = oneOf(state.tranchesShares.get(creator).keys())
        val withdrawLimitOrderMsg = {
            creator: creator,
            trancheKey: trancheKey
        }
        val res = withdrawLimitOrder(state, withdrawLimitOrderMsg)
        all {
            require(state.tranches.keys().contains(trancheKey)),
            // we are not allowing  withdrawing from fresh tranches (there was no trading)
            // @audit-info Should we handle the error at this level or should we let individual functions handle it?
            // This way is cleaner. The problem is that the simulation will not try at all then those cases where a withdraw on a non-fresh
            // tranche is attempted.
            // maybe it makes sense to use a `require` here at places where we can be confident that the code will enforce those assumptions
            require(not(state.tranches.get(trancheKey).isFresh)),
            state' = res.state,
            postconditionOK' = withdrawLimitOrderPostcondition(state, withdrawLimitOrderMsg, res.state),
            actionTaken' = {name: "withdrawLimitOrder", message: WithdrawLimitOrderSubType(withdrawLimitOrderMsg)},
            actionSuccess' = res.error == "nil"
            
        }
    
    
    //    CH: CANCELLING A LIMIT ORDER

    pure def cancelLimitOrderPostcondition(before: State, msg: CancelLimitOrderMsg, after: State) : bool =        
        val tranche_before: Tranche = before.tranches.get(msg.trancheKey)
        val users_tranches_before = before.tranchesShares.get(msg.creator).keys()
        val users_tranches_after = after.tranchesShares.get(msg.creator).keys()
        and {
            // there is at most one removed tranche from the set of users' tranches
            // and this is exactly the tranche that the user requested to be cancelled
            val removedTranches = users_tranches_before.exclude(users_tranches_after)
            and{
                size(removedTranches) <= 1,
                removedTranches.forall(
                    trancheKey => trancheKey == msg.trancheKey
                    )
            },
            
            // user received their fair amount of maker tokens back            
            // this calculation corresponds to the point 2a from the spec document
            // I am wondering if it is the same as `percentUnfilled * totalAmountOfUserShares` ==> pending confirmation from the dev team
            val percentUnfilled = calculatePercentUnfilled(before.tranches.get(msg.trancheKey))
            val userTotalTrancheShare = calculateUserTotalTrancheShare(tranche_before, before.tranchesShares.get(msg.creator))                    
            // @audit-info I am using "MakerToken" instead of "TokenIn". Is there a good reason to stick to "TokenIn" and "TokenOut" language?
            val cancelAmountOutMakerToken =                 
                computeCancelAmountMakerToken(percentUnfilled, userTotalTrancheShare, tranche_before.totalMaker)
            and{
                // the user's balance has increased for what was cancelled
                cancelAmountOutMakerToken == 
                after.coins.get(msg.creator).get(tranche_before.makerDenomination) -
                before.coins.get(msg.creator).get(tranche_before.makerDenomination),
                // the totalMaker has decreased for what was cancelled
                cancelAmountOutMakerToken ==
                before.tranches.get(msg.trancheKey).totalMaker -
                after.tranches.get(msg.trancheKey).totalMaker

            },
            
            // This part corresponds to the point 2b from the spec document
            val percentFilled = calculatePercentFilled(before.tranches.get(msg.trancheKey))     
            val userNonWithdrawnTrancheShare = calculateUserNonWithdrawnTrancheShare(tranche_before, before.tranchesShares.get(msg.creator))       
            val cancelAmountOutTakerToken = 
                // we are using "computeWITHDRAW..." because when cancelling, the user is also withdrawing their unwithdrawn share
                computeWithdrawTrancheAmountTakerToken(percentFilled, userNonWithdrawnTrancheShare, tranche_before.totalTaker)

            and{
                // the user's taker token balance has increased for what was withdrawn when cancelling
                cancelAmountOutTakerToken == 
                    after.coins.get(msg.creator).get(tranche_before.takerDenomination) -
                    before.coins.get(msg.creator).get(tranche_before.takerDenomination),
                // the totalTaker has decreased for what was cancelled
                cancelAmountOutTakerToken ==
                    before.tranches.get(msg.trancheKey).totalTaker -
                    after.tranches.get(msg.trancheKey).totalTaker
            },                        
        }



    pure def cancelLimitOrder(state: State, msg: CancelLimitOrderMsg) : Result =
        // 1) remove the user's tranches record
        val updatedUsersTranches = 
            state.tranchesShares.get(msg.creator).keys().exclude(Set(msg.trancheKey)).mapBy(
            trancheKey => state.tranchesShares.get(msg.creator).get(trancheKey)
            )

            
        

        val tranche = state.tranches.get(msg.trancheKey)
        // 2) update the user's coins
        // 2a) calculate the amount of the tranche maker token that the user should receive back
        val percentUnfilled = calculatePercentUnfilled(tranche)
        val cancelAmountMakerToken = mulInt(
            percentUnfilled,
            state.tranchesShares.get(msg.creator).get(msg.trancheKey).sharesOwned
        ).truncate()
        val updatedUserCoinsMaker = state.coins.get(msg.creator).get(msg.trancheKey.makerToken) + cancelAmountMakerToken
        // 2b) calculate the amount of the tranche taker token that the user should receive back (withdraw equivalent)
        val percentFilled = calculatePercentFilled(tranche)     
        val userNonWithdrawnTrancheShare = calculateUserNonWithdrawnTrancheShare(tranche, state.tranchesShares.get(msg.creator))       
        val cancelAmountTakerToken = 
            computeWithdrawTrancheAmountTakerToken(percentFilled, userNonWithdrawnTrancheShare, tranche.totalTaker)
        val updatedUserCoinsTaker = state.coins.get(msg.creator).get(msg.trancheKey.takerToken) + cancelAmountTakerToken
       
        // 3) update the tranche reserves
        val updatedTranche = {
            reservesMaker: tranche.reservesMaker - cancelAmountMakerToken,
            totalMaker: tranche.totalMaker - state.tranchesShares.get(msg.creator).get(msg.trancheKey).sharesOwned,
            reservesTaker: tranche.reservesTaker - cancelAmountTakerToken,
            totalTaker: tranche.totalTaker - cancelAmountTakerToken,
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


     action cancelLimitOrderAction: bool =
        // take one of the creators that have some tranches
        nondet creator = oneOf(state.tranchesShares.keys().filter(c => state.tranchesShares.get(c).keys().size() > 0))
        nondet trancheKey = oneOf(state.tranchesShares.get(creator).keys())
        val cancelLimitOrderMsg = {
            creator: creator,
            trancheKey: trancheKey
        }
        val res = cancelLimitOrder(state, cancelLimitOrderMsg)
        all{
            state' = res.state,
            postconditionOK' = cancelLimitOrderPostcondition(state, cancelLimitOrderMsg, res.state),
            actionTaken' = {name: "cancelLimitOrder", message: CancelLimitOrderSubType(cancelLimitOrderMsg)},
            actionSuccess' = res.error == "nil"
        }


    

    // Invariant: postconditions always hold
    val postconditions = postconditionOK

    // Invariant: pool reserves are always greater or equal to zero 
    val nonNegativeReserves = state.pools.keys().forall(poolId => val reserves = state.pools.get(poolId).reserves
                                                                  reserves._1 >= 0 and reserves._2 >= 0)

    // Invariant: shares supplies are always greater or equal to zero 
    val nonNegativeSharesSupply = state.pools.keys().forall(poolId => state.pools.get(poolId).shares >= 0)

    // Invariant: coin and share balances are zero or greater 
    val nonNegativeBalances = CREATORS.forall(
        creator => val coins = state.coins.get(creator)
                   val shares = state.poolShares.get(creator)
                   coins.keys().forall(token => coins.get(token) >= 0) and
                   shares.keys().forall(share => shares.get(share) >= 0))
    
    // Invariant: LP profit from their deposits
    // LP.5 in specification invariants
    val profitDeposit =
      CREATORS.forall(
        creator => state.deposits.get(creator).keys().forall(
            poolId => if (state.poolShares.get(creator).get(poolId) == 0)
                        val deposit = state.deposits.get(creator).get(poolId)
                        val withdrawnAmounts = state.poolWithdrawns.get(creator).get(poolId)
                        if (deposit.swaps) withdrawnAmounts._1 > deposit.amounts._1 and withdrawnAmounts._2 > deposit.amounts._2
                        else withdrawnAmounts._1 == deposit.amounts._1 and withdrawnAmounts._2 == deposit.amounts._2
                      else true))

    

    

    

    
    
    //     CH: ADVANCING TIME
    action advanceSingleBlockAction: bool =
        state' = {
            blockNumber: state.blockNumber + 1,
            ... state
        }

    action advanceMultipleBlocksAction: bool =
        nondet timestep = oneOf(1.to(MAX_FUTURE_BLOCKS))
        state' = {
            blockNumber: state.blockNumber + timestep,
            ... state
        }

    action advanceTimeAction: bool = 
        all{
            // separating the two (even though they could be combined) to
            // be able to explicitly turn off one of them
            any {
                advanceSingleBlockAction,
                advanceMultipleBlocksAction
            },
            postconditionOK' = postconditionOK,
            actionTaken' = {name: "advanceTime", message: AdvanceTime},
            actionSuccess' = true
        }        
        
        

    



}
