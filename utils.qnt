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


    // ------- OTHER
    
    pure def min(a,b) = if(a<b) a else b

    pure def priceFromTick(tick: int) : Dec =
        BASE_PRICE.pow(-tick)
    
    pure def ok(state: State): Result = {state: state, error: "nil"}

    pure def error(state: State, error: str): Result = {state: state, error: error}

    pure def initState : State = 
        {
            tranches: Map(),
            coins: CREATORS.mapBy(creator => TOKENS.mapBy(t => 0)),
            tranchesShares: CREATORS.mapBy(creator => Map()),
            blockNumber: 0,
            poolShares: Map(),
            pools: Map(),
            deposits: Map(),
            poolWithdrawns: Map()
        }

    // ----
        

    pure def unwrapTick(tick: Option[int]) : int = 
        match tick {
            | Some(t) => t
            | None => MAX_TICK
        }

    
    pure def computePoolValue(reserves: (int, int), tick: int): int = 
        val price1to0 = priceFromTick(-tick)
        reserves._1 + reserves._2.fromInt().mul(price1to0).truncate()

    pure def computeDepositValue(amounts: (int, int), tick: int): int = computePoolValue(amounts, tick)

    pure def tokenLT(t1: Token, t2: Token, token_num_values: Token -> int) : bool =
        // @audit-info What is the best practice to encode assumptions on pure functions,
        // without making the code too verbose?
        // assuming that both tokens are in the TOKEN_NUM_VALUES
        token_num_values.get(t1) < token_num_values.get(t2)


    pure def orderTokenPair(pair: TokenPair): TokenPair =
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
        exchangedTakerAmount: int,
        exchangedMakerAmount: int
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
                exchangedTakerAmount: amount,
                exchangedMakerAmount: tranche.reservesMaker - trancheRemainingMakerToken
            }
        else
            {
                trancheRemainingAmount: 0,
                exchangedTakerAmount: trancheAmountInTakerToken,
                exchangedMakerAmount: tranche.reservesMaker
            }                
    }
    
    pure def getMinimumTranche(tranches: Set[TrancheKey]): TrancheKey =
        tranches.fold(
            // tranches.chooseSome() ideally, but that one is still unimplemented in the simulation
            {pair: ("A", "B"), tick: MAX_TICK, id: 0},
            (acc, key) => 
                if (trancheLTE(key, acc))
                    key
                else 
                    acc
        )

 

    pure def getMinimumTrancheKey(trancheKeys: Set[TrancheKey], pair: TokenPair): TrancheKey =
        trancheKeys.fold(
            // trancheKeys.chooseSome() ideally, but that one is still unimplemented in the simulation
            {pair: pair, tick: MAX_TICK*10, id: 0},
            (acc, key) => 
                if (trancheLTE(key, acc)) 
                    key
                else 
                    acc
        )
    

    pure def getMinimumPoolKey(poolKeys: Set[PoolId], isPairNormalized: bool, pair: TokenPair): PoolId =
        val initTick = if (isPairNormalized) MAX_TICK else -MAX_TICK
        poolKeys.fold(
            // ideally, choose some, but doesn't work in the simulation
            {tokenPair: pair, tick: initTick, fee: 0},
            (acc, key) => 
                if (poolLTE(key, acc, isPairNormalized)) 
                    key
                else 
                    acc

        )

    
       

    // TRADE-RELATED FUNCTIONS
    // this function returns:
    //  - a mapping from poolIds to amounts that should be swapped with this pool
    //  - a mapping from trancheKeys to amounts that should be filled with this tranche
    //  - an integer saying how much of the order amount is still remaining
    // @audit-issue TODO types of limit orders
    // @audit-issue TODO call it swap
    pure def swap(
        pair: TokenPair,
        tranches: TrancheKey -> Tranche,
        pools: PoolId -> Pool,
        amount: int,
        maxPriceTick: int,
        maxAmountOut: int // in the first take, I will ignore this one
    ): {
        remainingAmount: int,
        outAmount: int, 
        tranchesUpdates: TrancheKey -> TrancheUpdateDiff, 
        poolUpdates: PoolId -> PoolUpdateDiff,
        outAmount: int
        } =
        
        // I need this for pools because they are stored only under normalized pairs
        val normalizedPair = orderTokenPair(pair)
        val isPairNormalized = pair == normalizedPair
        
        // @audit-info check if key.tick < maxPriceTick is the right semantics, or it should be on average
        // checked: it should be both.
        val relevantTranches = tranches.keys().filter(key => key.pair == pair and key.tick <= maxPriceTick)                
        val relevantPools = pools.keys()
                .filter(id => id.tokenPair == normalizedPair and computeReserveTick(id.tick, isPairNormalized, id.fee) <= maxPriceTick)            
            
        val relevantLiquiditySize = size(relevantTranches) + size(relevantPools)

        // the purpose of this fold is to iterate as many times as there are keys. But the iteration is not
        // happening over keys: instead, a minimum key is found in each iteration and the iteration continues.
        val foldResult = 1.to(relevantLiquiditySize).fold(
            {
                tranchesUpdates: Map(), 
                poolUpdates: Map(), 
                remainingAmountToExchange: amount,
                outAmount: 0,
                remainingTranches: relevantTranches, 
                remainingPools: relevantPools,
                outAmount: 0},
            (acc, _) =>
                // Iterate only if there is still amount to exchange
                if (acc.remainingAmountToExchange == 0)
                    acc
                else
                    // find me the minumum tranche key and the minimum pool key
                    val minTrancheKey: TrancheKey = getMinimumTrancheKey(acc.remainingTranches, pair)
                    val minPoolKey = getMinimumPoolKey(acc.remainingPools, isPairNormalized, pair)
                    // the pool price includes the fee
                    val minPoolReserveTick = computeReserveTick(minPoolKey.tick, isPairNormalized, minPoolKey.fee)
                    // use the tranche key only if its price is strictly lower than that of the pool
                    if (minTrancheKey.tick < minPoolReserveTick)                        
                        // I want to know how much I can exchange in this tranche 
                        val tranche = tranches.get(minTrancheKey)
                        val exchangeInfo = calculateTrancheExchangableAmount(acc.remainingAmountToExchange, tranche)
                        // update the acc value
                        {
                            tranchesUpdates: acc.tranchesUpdates.put(
                                minTrancheKey, 
                                {makerReservesDiff: -exchangeInfo.exchangedMakerAmount, takerReservesDiff: exchangeInfo.exchangedTakerAmount}),
                            remainingAmountToExchange: acc.remainingAmountToExchange - exchangeInfo.exchangedTakerAmount,
                            outAmount: acc.outAmount + exchangeInfo.exchangedTakerAmount,
                            remainingTranches: acc.remainingTranches.setRemove(minTrancheKey),
                            outAmount: acc.outAmount + exchangeInfo.exchangedMakerAmount,
                            ...acc
                        } 
                    // otherwise, I am using the pool
                    else
                        val pool = pools.get(minPoolKey)
                        // tokenB is the one that the user will be getting
                        val reservesTokenB = if (isPairNormalized) pool.reserves._2 else pool.reserves._1   
                        val exchangeRate = priceFromTick(minPoolReserveTick)                            
                        val requiredAmount = mulInt(exchangeRate, acc.remainingAmountToExchange).truncate()                        
                        val userGetsFromPool = if (requiredAmount <= reservesTokenB) requiredAmount else  reservesTokenB
                        // @audit-issue should we take into account numerical considerations here and instead of dividing,
                        // calculate the opposite price tick and multiply?
                        val userPutsIntoPool = intDivDec(userGetsFromPool, exchangeRate).truncate()

                        {
                            remainingPools: acc.remainingPools.setRemove(minPoolKey),
                            remainingAmountToExchange: acc.remainingAmountToExchange - userPutsIntoPool,
                            outAmount: acc.outAmount + userGetsFromPool,
                            poolUpdates: acc.poolUpdates.set(
                                minPoolKey, 
                                if (isPairNormalized) 
                                    {token0Diff: userPutsIntoPool, token1Diff: -userGetsFromPool} 
                                else 
                                    {token0Diff: -userGetsFromPool, token1Diff: userPutsIntoPool}
                                ),
                            outAmount: acc.outAmount + userGetsFromPool,
                            ...acc
                        }
                        
        )
        {remainingAmount: foldResult.remainingAmountToExchange, outAmount: foldResult.outAmount, poolUpdates: foldResult.poolUpdates, tranchesUpdates: foldResult.tranchesUpdates}
        
        

    

    pure def poolSwap(state: State, poolId: PoolId, poolUpdateDiff: PoolUpdateDiff) : State = 
        state

    pure def trancheTrade(state: State, trancheKey: TrancheKey, trancheUpdateDiff: TrancheUpdateDiff) : State = state

    pure def handleRemainingLimitOrderAmount(state: State, msg: PlaceLimitOrderMsg, amount: int) : State =
        val tranches = state.tranches
        val tokenPair = msg.tokenPair
        val tick = msg.tick
        val shares = state.tranchesShares
        val creator = msg.creator
        if (amount <= 0)
            state
        else
            // looking for an existing tranche under which we would place the maker order 
            val appropriateTrancheKeys = tranches.keys().filter(key => key.pair == tokenPair and key.tick == tick and tranches.get(key).isFresh)            
            val tranchesAndSharesUpdate = 
                if (size(appropriateTrancheKeys) > 0)
                    val trancheKey = getMinimumTrancheKey(appropriateTrancheKeys, tokenPair)                
                    val updatedTranches = tranches.setBy(trancheKey, old => {reservesMaker: old.reservesMaker + amount, ...old})
                    val updatedShares = shares.setBy(
                        creator,
                        (old => old.set(
                            trancheKey, 
                            {sharesOwned: old.get(trancheKey).sharesOwned + amount, ...old.get(trancheKey)}
                            )
                        )
                    )
                    (updatedTranches, updatedShares)

                
                else
                    val maxTrancheId = tranches.keys().fold(
                        0,
                        (acc, key) => if (key.id > acc) key.id else acc
                    )
                    // in the implementation, the id is connected to the block number. Here, we use increasing values
                    val newKey = {pair: tokenPair, tick: tick, id: maxTrancheId + 1}
                    val newTranche = {
                        isFresh: true,
                        reservesMaker: amount,
                        reservesTaker: 0,
                        totalMaker: amount,
                        totalTaker: 0,
                        tick: tick,
                        // TODO: this is not implemented
                        expirationTime: -1,
                        trancheKey: newKey,
                        makerDenomination: tokenPair._1,
                        takerDenomination: tokenPair._2
                    }
                    val newShareAccount = {sharesOwned: amount, sharesWithdrawn: 0}

                    
                    val updatedTranches = tranches.put(newKey, newTranche)
                    val updatedShares = shares.setBy(
                            creator,
                            old => old.put(newKey, newShareAccount)
                        )
                    (updatedTranches, updatedShares)
                // returning the state
                val updatedCoins = state.coins.setBy(
                    msg.creator, 
                    // we spent all coins! This is specific for the Good-Til-Cancelled. TODO: handle other types
                    old => old.set(msg.tokenPair._1, 0)                    
                    )
                {
                    tranches: tranchesAndSharesUpdate._1,
                    tranchesShares: tranchesAndSharesUpdate._2,
                    coins: updatedCoins,
                    ...state
                }
                
                




    // DEPOSIT RELATED FUNCTIONS
    pure def computeAmountsDeposit(reserves: (int, int), amounts: (int, int)): (int, int) = {
      val inAmount0 = if (reserves._2 > 0)
                        min(amounts._1, amounts._2.fromInt().mul(reserves._1.fromInt()).div(reserves._2.fromInt()).truncate())
                      else amounts._1
      val inAmount1 = if (reserves._1 > 0)
                        min(amounts._2, amounts._1.fromInt().mul(reserves._2.fromInt()).div(reserves._1.fromInt()).truncate())
                      else amounts._2
      (inAmount0, inAmount1)
    }

    // LP.2 in specification invariants
    // autoswapFee = residualValue0 * (1 - 1.0001^-fee) + residualValue1 * (1 - 1.0001^-fee) * price1To0
    pure def computeAutoswapFee(residualAmounts: (int, int), tick: TickIndex): Dec =
        computeDepositValue(residualAmounts, tick).fromInt().mul(one.sub(priceFromTick(tick)))

    // @audit-info not sure if to compute depositValue when autoswapping I should use the full amount in the
    // deposit message or just the one computed by computeAmountsDeposit
    // can totalPoolValue be 0? Invariant candidate!
    // can autoswapFee > depositValue? Invariant candidate!
    // 
    // LP.1 and LP.2 in specification invariants
    // shareValue = totalPreexistingShares / (totalPoolValue + autswapFee)
    // sharesMinted = (depositValue - autoswapFee) * shareValue
    pure def computeSharesDeposit(existingShares: int, reserves: (int, int), amounts: (int, int), tick: int, autoswapFee: Dec): int =
      val poolValue = computePoolValue(reserves, tick)
      val shareValue = existingShares.fromInt().div(poolValue.fromInt().add(autoswapFee)).truncate()
      val depositValue = computeDepositValue(amounts, tick)
      depositValue.fromInt().sub(autoswapFee).mul(shareValue.fromInt()).truncate()

    // This specifies the implementation
    // fee = (upperTickIndex - lowerTickIndex) / 2
    // residualShares = residualAmounts0 * (Price1to0Center / Price1to0Upper) + residualAmounts1 * Price1to0Lower
    pure def computeResidualShares(residualAmounts: (int, int), id: PoolId): int = 
      val upperTickIndex = id.tick + id.fee
      val lowerTickIndex = -id.tick + id.fee
      val fee = (upperTickIndex - lowerTickIndex) / 2
      val amount0Discount = priceFromTick(-fee)
      val priceLowerTakerToMaker = priceFromTick(-id.tick + id.fee)
      amount0Discount.mul(residualAmounts._1.fromInt()).add(priceLowerTakerToMaker.mul(residualAmounts._2.fromInt())).truncate()

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

    pure def getPoolId(tokens: (Token, Token), tick: TickIndex, fee: Fee): PoolId = 
      {tokenPair: tokens, tick: tick, fee: fee}

    pure def trancheLTE(t1: TrancheKey, t2: TrancheKey): bool = 
        // assume here: t1.pair == t2.pair; otherwise, unordered
        if (t1.tick == t2.tick)
            t1.id <= t2.id
        else
            t1.tick <= t2.tick

    pure def poolLTE(p1: PoolId, p2: PoolId, isPairNormalized: bool): bool =
        val tick1 = computeReserveTick(p1.tick, isPairNormalized, p1.fee)
        val tick2 = computeReserveTick(p2.tick, isPairNormalized, p2.fee)
        // if the ticks are the same
        if (tick1 == tick2)
            // then order by fee
            p1.fee <= p2.fee
        else
            // otherwise, order by ticks
            tick1 <= tick2

    

      

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

    pure def computeReserveTick(poolTick: TickIndex, token0to1: bool, fee: Fee): TickIndex = 
        if (token0to1) 
            poolTick + fee 
        else 
            -poolTick + fee

    // @audit-issue Potentially, we need to add a break after we are past limitPrice --> checking with the dev team
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

    pure def updateSwapsBool(user: Addr, poolIds: Set[PoolId], userDeposits: PoolId -> {amounts: (int, int), swaps: bool}, userShares: PoolId -> int) :
      PoolId -> {amounts: (int, int), swaps: bool} = 
        poolIds.fold(userDeposits, (acc, id) => if (userDeposits.has(id) and userShares.get(id) > 0)
                                                    acc.set(id, {swaps: true, ...userDeposits.get(id)})
                                                else acc)


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

    