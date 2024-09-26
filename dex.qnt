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
    }


    
    
    
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
        val updatedPools = updatePools(state.pools, poolsUpdateInfo)
        

        // 2) update the tranches states
        val tradeUpdatedTranches = updateTranches(state.tranches, tranchesUpdateInfo)
        

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
        // @audit-inf TODO: clearing of expired orders missing
        val updatedState = {blockNumber: state.blockNumber + numBlocks, ...state}
        ok(updatedState)
    }

}
