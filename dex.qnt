// -*- mode: Bluespec; -*-

/**
 * The core of the Neutron DEX.
 */
module Dex {
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"
    import utils.* from "utils"
    import bookkeeping.* from "bookkeeping"
    import basicSpells.* from "lib/basicSpells"

    pure def withdrawPool(state: State, msg: WithdrawPoolMsg): Result = {
        // deposits and poolWithdraws variable management
        //   @audit-info These three lines are a candidate to be replaced throughout the whole codebase
        val pair = orderTokenPair(msg.tokenPair)
        val tick = if (pair == msg.tokenPair) msg.tick else -msg.tick
        val poolId = getPoolId(pair, tick, msg.fee)
        // pool must exist
        val pool = state.pools.get(poolId)
        val outAmounts = computeAmountsWithdrawnPool(pool.reserves, pool.shares, msg.sharesAmount)

        val updatedReserves = (pool.reserves._1 - outAmounts._1, pool.reserves._2 - outAmounts._2)
        val updatedShares = pool.shares - msg.sharesAmount
        
        val updatedCoinsForCreator = state.coins.get(msg.creator)
            .setBy(pair._1, old => old + outAmounts._1)
            .setBy(pair._2, old => old + outAmounts._2)
        val updatedPoolSharesForCreator = state.poolShares.get(msg.creator)
            .setBy(poolId, old => old - msg.sharesAmount)

        ok({
            pools: state.pools.set(poolId, {reserves: updatedReserves, shares: updatedShares}),
            coins: state.coins.set(msg.creator, updatedCoinsForCreator),
            poolShares: state.poolShares.set(msg.creator, updatedPoolSharesForCreator),
            bookkeeping: state.bookkeeping.trackPoolWithdrawal(msg.creator, poolId, outAmounts, state.poolShares),
            ...state
        })
    }

  
    pure def deposit(state: State, msg: DepositMsg): Result = {
        // first, sorting
        val pair = orderTokenPair(msg.tokenPair)
        val amounts = if (pair == msg.tokenPair) msg.amountPair else (msg.amountPair._2, msg.amountPair._1)
        val token0SellTick = if (pair == msg.tokenPair) msg.token0SellTick else -msg.token0SellTick

        // @audit-info Consider not checking this, but disallowing any executions that do not satisfy the property
        if (isPoolBehindEnemyLines(state, pair, token0SellTick, msg.fee, amounts)) { state: state, error: "Pool behind enemy lines" } else
        
        val poolId = getPoolId(pair, token0SellTick, msg.fee)
        val pool = if (state.pools.has(poolId)) state.pools.get(poolId) else emptyPool
        val inAmountsBeforeAutoswap = computeAmountsDeposit(pool.reserves, amounts)
        val residualAmounts = (amounts._1 - inAmountsBeforeAutoswap._1, amounts._2 - inAmountsBeforeAutoswap._2)
        val performAutoswap: bool = msg.autoswap and (residualAmounts._1 > 0 or residualAmounts._2 > 0)
        val inAmounts = if (performAutoswap) amounts else inAmountsBeforeAutoswap
        val autoswapFeeValue = if (performAutoswap) computeAutoswapFeeValue(residualAmounts, token0SellTick, msg.fee) else zero
        val outShares = computeSharesDeposit(pool.shares, pool.reserves, inAmounts, token0SellTick, autoswapFeeValue)

        if (outShares == 0) { state: state, error: "No shares created" } else

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
        val updatedCreatorCoins = state.coins.get(msg.creator)
            .setBy(pair._1, old => old - inAmounts._1)
            .setBy(pair._2, old => old - inAmounts._2)

        ok({
            poolShares: updatedShares,
            pools: updatedPools,
            coins: state.coins.set(msg.creator, updatedCreatorCoins),
            bookkeeping: state.bookkeeping.trackPoolDeposit(msg.creator, poolId, inAmounts, (if (performAutoswap) Some(residualAmounts) else None), state.poolShares),
            ...state
        })
    }
    
    pure def placeLimitOrder(state: State, msg: PlaceLimitOrderMsg): Result =        
        val maxBuyTick = - msg.sellTick
        val fulfilledOrderInfo = swap(
            (msg.tokenIn, msg.tokenOut),
            state.tranches,
            state.pools,
            msg.amountIn,
            maxBuyTick,
            msg.maxAmountOut            
        )
        
        val poolsUpdateInfo = fulfilledOrderInfo.poolUpdates
        val tranchesUpdateInfo = fulfilledOrderInfo.tranchesUpdates
        val remainingAmount = fulfilledOrderInfo.remainingAmount
        val tokenOutReceived = fulfilledOrderInfo.outAmount

        if (remainingAmount > 0 and msg.orderType == FillOrKill) { state: state, error: "FillOrKill: not enough liquidity" } else
     
        // 1) update the pools states
        val updatedPools = updatePools(state.pools, poolsUpdateInfo)

        // 2) update the tranches states
        val tradeUpdatedTranches: TrancheKey->Tranche = updateTranches(state.tranches, tranchesUpdateInfo)
        // 2a) handle the remaining amount and update tranches accordingly
        val tranchesSharesUpdate = 
            // no new updates to tranches if the type is ImmediateOrCancel, or 
            // if the remaining amount is 0
            if (msg.orderType == ImmediateOrCancel or remainingAmount == 0) 
                { tranches: tradeUpdatedTranches, tranchesShares: state.tranchesShares, key: None }
            else //if there is something to be placed
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

        ok({
            tranches: tranchesSharesUpdate.tranches,
            tranchesShares: tranchesSharesUpdate.tranchesShares,
            pools: updatedPools,
            coins: state.coins.set(msg.creator, updatedUserCoins),
            bookkeeping: state.bookkeeping
              .trackTrancheDeposit(msg.creator, tranchesSharesUpdate.key, remainingAmount)
              .trackSwap(state.tranches, state.tranchesShares,  state.poolShares, fulfilledOrderInfo),
            ...state
        })

    pure def singlehopSwap(state: State, msg: SwapMsg): Result = {
      val inAmount = msg.amount
      val fulfilledOrderInfo = swap(
            msg.tokenPair,
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


        ok({
            coins: state.coins.set(msg.creator, updatedCoins),
            pools: updatedPools,
            tranches: updatedTranches,
            bookkeeping: state.bookkeeping.trackSwap(state.tranches, state.tranchesShares, state.poolShares, fulfilledOrderInfo),
            ...state
        })
    }

    pure def withdrawLimitOrder(state: State, msg: WithdrawLimitOrderMsg): Result =
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
            bookkeeping: state.bookkeeping.trackTrancheWithdrawal(msg.creator, msg.trancheKey, withdrawnTaker),
            ...state
        }
        ok(updatedState)

    pure def cancelLimitOrder(state: State, msg: CancelLimitOrderMsg): Result =
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
            bookkeeping: state.bookkeeping.trackTrancheCancelation(msg.creator, msg.trancheKey, cancelAmountMakerToken, withdrawAmountTakerToken),
            ... state
        }
        ok(updatedState)
    
    pure def advanceBlocks(state: State, numBlocks: int): Result = {
        val newBlock = state.blockNumber + numBlocks
        val updatedTranches = state.tranches.keys()
            .mapBy(k =>
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
