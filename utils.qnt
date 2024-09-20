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
    
    pure def computeAmountFromPrice(usedAmount: int, price: Dec): int = 0

    pure def unwrapTick(tick: Option[int]) : int = 
        match tick {
            | Some(t) => t
            | None => MAX_TICK
        }

    

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
    
    // given an amount in the taker token, return how much of it the tranche can exchange and the update amount of the tranche
    pure def calculateTrancheExchangableAmount(amount: int, tranche: Tranche): 
    {
        trancheRemainingAmount: int,
        exchangedTakerAmount: int
    } = {
        
        val priceTakerToMaker = priceFromTick(tranche.tick)
        val trancheAmountInTakerToken: int = 
            intDivDec(
                tranche.reservesMaker,
                priceTakerToMaker
            ).truncate()
        
        if (amount <= trancheAmountInTakerToken) 
            val trancheRemainingTakerToken: int = trancheAmountInTakerToken - amount
            val trancheRemainingMakerToken = mulInt(priceTakerToMaker, trancheRemainingTakerToken).truncate()
            
            {
                trancheRemainingAmount: trancheRemainingMakerToken,
                exchangedTakerAmount: amount
            }
        else
            {
                trancheRemainingAmount: 0,
                exchangedTakerAmount: trancheAmountInTakerToken
            }                
    }
        

    pure def getMinimumTrancheKey(trancheKeys: Set[TrancheKey]): TrancheKey =
        trancheKeys.fold(
            // trancheKeys.chooseSome() ideally, but that one is still unimplemented in the simulation
            {pair: ("A", "B"), tick: MAX_TICK, id: 0},
            (acc, key) => 
                if (trancheLTE(key, acc)) 
                    key
                else 
                    acc
        )

    pure def tranchesLiquidityIteration(
        pair: TokenPair,
        tranches: TrancheKey -> Tranche,
        amount: int,
        maxPriceTick: int
    ): {tranchesUpdates: TrancheKey -> int, remainingAmount: int} =
        if (amount == 0)
            {tranchesUpdates: Map(), remainingAmount: 0}
        else
            val relevantTranches = tranches.keys().filter(key => key.pair == pair and key.tick <= maxPriceTick)

            // the purpose of this fold is to iterate as many times as there are keys. But the iteration is not
            // happening over keys: instead, a minimum key is found in each iteration and the iteration continues.
            val foldResult = relevantTranches.fold(
                {tranchesUpdates: Map(), remainingAmount: amount, remainingTranchesKeys: relevantTranches},
                (acc, _) =>
                    // Iterate only if there is still amount to exchange
                    if (acc.remainingAmount == 0)
                        acc
                    else
                        // find me the minumum tranche key
                        val minTrancheKey = getMinimumTrancheKey(acc.remainingTranchesKeys) 
                        
                        // I want to know how much I can exchange in this tranche 
                        val tranche = tranches.get(minTrancheKey)
                        val exchangeInfo = calculateTrancheExchangableAmount(acc.remainingAmount, tranche)
                        // update the acc value
                        {
                            tranchesUpdates: acc.tranchesUpdates.set(minTrancheKey, exchangeInfo.trancheRemainingAmount),
                            remainingAmount: acc.remainingAmount - exchangeInfo.exchangedTakerAmount,
                            remainingTranchesKeys: acc.remainingTranchesKeys.setRemove(minTrancheKey)
                        }                                    
            )
            {tranchesUpdates: foldResult.tranchesUpdates, remainingAmount: foldResult.remainingAmount}
       

    // TRADE-RELATED FUNCTIONS
    // this function returns:
    //  - a mapping from poolIds to amounts that should be swapped with this pool
    //  - a mapping from trancheKeys to amounts that should be filled with this tranche
    //  - an integer saying how much of the order amount is still remaining
    pure def computeMakerOrderMatches(state: State, msg: PlaceLimitOrderMsg) : (PoolId->(int, int), TrancheKey-> int, int) = 
    {
        
        // first, exchange using the pools on the relevant tick (they are the first according to the sort)
        val poolIterationRes = poolsLiquidityIteration(msg.tokenPair, true, state.pools, msg.amountIn, msg.tick)
        val remainingAmountAfterPoolIteration = poolIterationRes.inAmount

        // second, exchange using the tranches on the relevant tick
        val trancheIterationRes = tranchesLiquidityIteration(msg.tokenPair, state.tranches, remainingAmountAfterPoolIteration, msg.tick)
        (poolIterationRes.changes, Map(), -1)

        // finally, use the remainign amount to place a new maker order

    }

    pure def poolSwap(state: State, poolId: PoolId, amounts: (int,int)) : State = state

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
    pure def computeSharesDeposit(existingShares: int, reserves: (int, int), amounts: (int, int), price1to0: Dec): int =
      val totalPoolValue = reserves._1 + reserves._2.fromInt().mul(price1to0).truncate()
      val shareValue = existingShares.fromInt().div(totalPoolValue.fromInt()).truncate()
      val depositValue = amounts._1 + amounts._2.fromInt().mul(price1to0).truncate()
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

        
    pure def computeAmountsWithdrawPool(reserves: (int, int), totalShares: int, shares: int): (int, int) =
      val proRataShare = totalShares.fromInt().div(shares.fromInt()).truncate()
      (proRataShare * reserves._1, proRataShare * reserves._2)

    pure def getPoolId(tokens: (Token, Token), tick: TickIndex, fee: Fee): PoolId = 
      {tokenPair: tokens, tick: tick, fee: fee}

    pure def trancheLTE(t1: TrancheKey, t2: TrancheKey): bool = 
        // assume here: t1.pair == t2.pair; otherwise, unordered
        if (t1.tick == t2.tick)
            t1.id <= t2.id
        else
            t1.tick <= t2.tick

      

    pure def minTickPool(ids: Set[PoolId], token0to1: bool): PoolId =
      val maxPoolId = {tokenPair: ids.chooseSome().tokenPair, tick: MAX_TICK, fee: MAX_FEE}
      ids.fold(
        maxPoolId, 
        (acc, id) => 
            if (token0to1 and id.tick + id.fee < acc.tick + acc.fee) 
                id
            else if (not(token0to1) and -id.tick + id.fee < -acc.tick + acc.fee) 
                id
            else 
                acc 
    )

    pure def computeReserveTick(tick: TickIndex, token0to1: bool, fee: Fee): TickIndex = 
        if (token0to1) 
            tick + fee 
        else 
            -tick + fee

    // TODO: make the token0to1 clearer
    // TODO: add ordering over minimum fee
    pure def poolsLiquidityIteration(
        pair: (Token, Token), 
        token0to1: bool, 
        pools: PoolId -> Pool, 
        amount: int,
        maxPriceTick: int
        ): 
      {remainingPools: Set[PoolId], inAmount: int, outAmount: int, changes: PoolId -> (int, int)} =
        
            
        // val token0to1 = pair._1 == tokenA
        val relevantPools = pools.keys().filter(id => id.tokenPair == pair and id.tick <= maxPriceTick)
        relevantPools.fold(
            {remainingPools: relevantPools, inAmount: amount, outAmount: 0, changes: relevantPools.mapBy(pool => (0, 0))}, 
            (acc, id) =>
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
                    remainingPools: acc.remainingPools.setRemove(id),
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

    