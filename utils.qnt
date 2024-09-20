// -*- mode: Bluespec; -*-

module utils {
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"
    import basicSpells.* from "basicSpells"

    

    // TRANCHE related functions
    pure def calculatePercentFilled(t: Tranche) : Dec =
        fromRatio(t.totalMaker - t.reservesMaker, t.totalMaker)
    
    pure def calculatePercentUnfilled(t: Tranche) : Dec =
        fromRatio(t.reservesMaker, t.totalMaker)
    
    pure def calculateUserTotalTrancheShare(t: Tranche, users_share_account: ShareAccount) : Dec =
        val userShare = users_share_account.get(t.trancheKey).sharesOwned
        fromRatio(userShare, t.totalMaker)

    pure def calculateUserNonWithdrawnTrancheShare(t: Tranche, users_share_account: ShareAccount) : Dec =
        val userShare = users_share_account.get(t.trancheKey).sharesOwned - users_share_account.get(t.trancheKey).sharesWithdrawn
        fromRatio(userShare, t.totalMaker)


    // -------
    
    pure def min(a,b) = if(a<b) a else b

    pure def priceFromTick(tick: int) : Dec =
        BASE_PRICE.pow(-tick)
    
    pure def ok(state: State): Result = {state: state, error: "nil"}

    pure def error(state: State, error: str): Result = {state: state, error: error}
    
    pure def getPoolId(tokens: (Token, Token), tick: TickIndex, fee: Fee): PoolId = 
      {tokenPair: tokens, tick: tick, fee: fee}

    pure def computePoolValue(reserves: (int, int), tick: int): int = 
        val price1to0 = priceFromTick(-tick)
        reserves._1 + reserves._2.fromInt().mul(price1to0).truncate()

    pure def computeDepositValue(amounts: (int, int), tick: int): int = computePoolValue(amounts, tick)

    pure def tokenLT(t1: Token, t2: Token, token_num_values: Token -> int) : bool =
        // @audit-info What is the best practice to encode assumptions on pure functions,
        // without making the code too verbose?
        // assuming that both tokens are in the TOKEN_NUM_VALUES
        token_num_values.get(t1) < token_num_values.get(t2)


    pure def orderTokenPair(pair: (Token, Token)): (Token, Token) =
      if (tokenLT(pair._1, pair._2, TOKEN_NUM_VALUES)) pair else (pair._2, pair._1)

    pure def normalizeCoinAndTick(tokenPair: (Token, Token), tick: int) : ((Token, Token), int) =
        val tokenIn = tokenPair._1
        val tokenOut = tokenPair._2
        if (tokenLT(tokenIn, tokenOut, TOKEN_NUM_VALUES)) 
            ((tokenIn, tokenOut), tick)
        else 
            ((tokenOut, tokenIn), -tick)         
    
    
    
    pure def calculateTrancheIndex(tokenPair: TokenPair, tick: TickIndex, existingTranchesKeys: Set[TrancheKey]): int = 
    
        // TODO: NOT-IMPLEMENTED
        -1
    
    // TRADE-RELATED FUNCTIONS
    // this function returns:
    //  - a mapping from poolIds to amounts that should be swapped with this pool
    //  - a mapping from trancheKeys to amounts that should be filled with this tranche
    //  - an integer saying how much of the order amount is still remaining
    pure def computeMakerOrderMatches(state: State, msg: PlaceLimitOrderMsg) : (PoolId->int, TrancheKey-> int, int) = 
        (Map(), Map(), -1)

    pure def poolSwap(state: State, poolId: PoolId, amount: int) : State = state

    pure def trancheTrade(state: State, trancheKey: TrancheKey, amount: int) : State = state

    pure def createNewTranche(state: State, msg: PlaceLimitOrderMsg, amount: int) : State = state

    // DEPOSIT RELATED FUNCTIONS
    // @audit-info there's a Quint error I cannot resolve
    pure def computeAmountsDeposit(reserves: (int, int), amounts: (int, int)): (int, int) = {
      val inAmount0 = if (reserves._2 > 0)
                        min(amounts._1, amounts._2.fromInt().mul(reserves._1.fromInt()).div(reserves._2.fromInt()).truncate())
                      else amounts._1
      val inAmount1 = if (reserves._1 > 0)
                        min(amounts._2, amounts._1.fromInt().mul(reserves._2.fromInt()).div(reserves._1.fromInt()).truncate())
                      else amounts._2
      (inAmount0, inAmount1)
    }

    // @audit-info when to truncate?
    // can totalPoolValue be 0? Invariant candidate!
    // LP.1 in specification invariants
    pure def computeSharesDeposit(existingShares: int, reserves: (int, int), amounts: (int, int), tick: int): int =
      val poolValue = computePoolValue(reserves, tick)
      val shareValue = existingShares.fromInt().div(poolValue.fromInt()).truncate()
      val depositValue = computeDepositValue(amounts, tick)
      depositValue * shareValue

    // WITHDRAW TRANCHE RELATED FUNCTIONS
    pure def computeWithdrawTrancheShares(percentFilled: Dec, userNonWithdrawnTrancheShare: Dec): int =
        // @audit-issue Should this be truncate? Rounding discussion
        truncate(
            mul(
                percentFilled,
                userNonWithdrawnTrancheShare
            )
        )

    pure def computeWithdrawTrancheAmountTakerToken(percentFilled: Dec, userNonWithdrawnTrancheShare: Dec, totalTaker: int): int = 
        truncate(
            mulInt(
                mul(
                    percentFilled,
                    userNonWithdrawnTrancheShare
                ),
                totalTaker
            )
        )

    // WITHDRAW POOL RELATED FUNCTIONS

    // It also mimics the implementation
    // Assumes that totalShares > 0, which is checked by the invariants
    // LP.4 in specification invariants
    pure def computeAmountsWithdrawPool(reserves: (int, int), totalShares: int, shares: int): (int, int) =
      val proRataShare = shares.fromInt().div(totalShares.fromInt()).truncate()
      (proRataShare * reserves._1, proRataShare * reserves._2)

    pure def minTickPool(ids: Set[PoolId], token0to1: bool): PoolId =
      val maxPoolId = {tokenPair: ids.chooseSome().tokenPair, tick: MAX_TICK, fee: MAX_FEE}
      ids.fold(
        maxPoolId, 
        (acc, id) => 
            if (token0to1 and id.tick + id.fee < acc.tick + acc.fee) 
                id
            else if (token0to1 and id.tick + id.fee == acc.tick + acc.fee and id.fee < acc.fee) 
                id
            else if (not(token0to1) and -id.tick + id.fee < -acc.tick + acc.fee) 
                id
            else if (not(token0to1) and -id.tick + id.fee == -acc.tick + acc.fee and id.fee < acc.fee) 
                id
            else 
                acc 
    )

    pure def computeReserveTick(tick: TickIndex, token0to1: bool, fee: Fee): TickIndex = 
        if (token0to1) 
            tick + fee 
        else 
            -tick + fee


    pure def poolsLiquidityIteration(pair: (Token, Token), tokenA: Token, pools: PoolId -> Pool, amount: int): 
      {remainingPools: Set[PoolId], inAmount: int, outAmount: int, changes: PoolId -> (int, int)} =
        val token0to1 = pair._1 == tokenA
        val relevantPools = pools.keys().filter(id => id.tokenPair == pair)
        1.to(size(relevantPools)).fold(
            {remainingPools: relevantPools, inAmount: amount, outAmount: 0, changes: relevantPools.mapBy(pool => (0, 0))}, 
            (acc, _) =>
                // Compute cheapest exchange rate among the remaining pools
                val poolId =  minTickPool(acc.remainingPools, token0to1)
                val reserveTick = computeReserveTick(poolId.tick, token0to1, poolId.fee)
                val exchangeRate = priceFromTick(reserveTick)
                // Compute the inAmount and outAmount from the previously selected pool
                val reservesTokenB = if (token0to1) pools.get(poolId).reserves._2 else pools.get(poolId).reserves._1
                val requiredAmount = acc.inAmount.fromInt().mul(exchangeRate).truncate()
                val inAmount = if (requiredAmount <= reservesTokenB) acc.inAmount else reservesTokenB
                val outAmount = inAmount.fromInt().mul(exchangeRate).truncate()
                // update the acc value
                {
                    remainingPools: acc.remainingPools.setRemove(poolId),
                    inAmount: acc.inAmount - inAmount,
                    outAmount: acc.outAmount + outAmount,
                    changes: acc.changes.set(poolId, if (token0to1) (inAmount, outAmount) else (outAmount, inAmount))
                }
        )



    // CANCEL RELATED FUNCTIONS
    pure def computeCancelAmountMakerToken(percentUnfilled: Dec, userTotalTrancheShare: Dec, totalMaker: int): int =
        // @audit-issue Already here we are running into rounding problems. Would be great to get the description on rounding
        // that we discussed in one of the previous meetings
        truncate(
            mulInt(
                mul(
                    percentUnfilled,
                    userTotalTrancheShare
                ),
                totalMaker
            )
        )

}

    