// -*- mode: Bluespec; -*-

module utils {
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"
    import basicSpells.* from "lib/basicSpells"

    pure def debug(msg, expr) =
        if (DEBUG_ENABLED)
            q::debug(msg, expr)
        else
            expr

    // TRANCHE related functions
    pure def calculatePercentFilled(t: Tranche): Dec =
        if (t.totalMaker == 0)
            one
        else
            fromRatio(t.totalMaker - t.reservesMaker, t.totalMaker)
    
    pure def calculatePercentUnfilled(t: Tranche): Dec =
        if (t.totalMaker == 0)
            zero
        else
            fromRatio(t.reservesMaker, t.totalMaker)
    
    pure def calculateUserTotalTrancheShare(t: Tranche, users_share_account: ShareAccount): Dec =
        val userShare = users_share_account.get(t.trancheKey).sharesOwned
        if (t.totalMaker == 0)
            zero
        else
            fromRatio(userShare, t.totalMaker)

    pure def calculateUserNonWithdrawnTrancheShare(t: Tranche, users_share_account: ShareAccount): Dec =
        val userShare = users_share_account.get(t.trancheKey).sharesOwned - users_share_account.get(t.trancheKey).sharesWithdrawn
        if (t.totalMaker == 0)
            zero
        else
            fromRatio(userShare, t.totalMaker)

    // ------- OTHER
    
    pure def min(a,b) = if (a<b) a else b

    pure def priceFromTick(tick: int): Dec =
        BASE_PRICE.pow(-tick)
    
    pure def ok(state: State): Result = {state: state, error: "nil"}

    pure def error(state: State, error: str): Result = {state: state, error: error}

    pure def initState: State =
        // @audit-issue This should be the correct spec, but replaced by a simpler, hard-coded one due to
        // a problem in the simulator: https://github.com/informalsystems/quint/issues/1530
        // val possibleTokenAccounts = setOfMaps(CREATORS, setOfMaps(TOKENS, INITIAL_CREATORS_BALANCES))
        // nondet initCoins = oneOf(possibleTokenAccounts)
        val bal = INITIAL_CREATOR_BALANCE_TEMP
        val initCoins = CREATORS.fold(Map(), (acc, c) => acc.put(c, TOKENS.fold(
            Map(), (acc2, t) => acc2.put(t, bal)
        )))

        {
            tranches: Map(),
            coins: initCoins,
            tranchesShares: CREATORS.mapBy(creator => Map()),
            blockNumber: 0,
            poolShares: CREATORS.mapBy(creator => Map()),
            pools: Map(),
            deposits: CREATORS.mapBy(creator => Map()),
            poolWithdraws: CREATORS.mapBy(creator => Map())
        }

    // ----

    pure def unwrapTick(tick: Option[int]): int = 
        match tick {
            | Some(t) => t
            | None => MAX_TICK
        }

    pure def computePoolValue(reserves: (int, int), tick0To1: int): Dec = 
        val tick1To0 = -tick0To1
        val price1to0 = priceFromTick(tick1To0)
        fromInt(reserves._1).add(mulInt(price1to0, reserves._2))

    pure def computeDepositValue(amounts: (int, int), tick0To1: int): Dec = computePoolValue(amounts, tick0To1)

    pure def tokenLT(t1: Token, t2: Token, token_num_values: Token -> int): bool =
        // @audit-info What is the best practice to encode assumptions on pure functions,
        // without making the code too verbose?
        // assuming that both tokens are in the TOKEN_NUM_VALUES
        token_num_values.get(t1) < token_num_values.get(t2)

    pure def orderTokenPair(pair: TokenPair): TokenPair =
        if (tokenLT(pair._1, pair._2, TOKEN_NUM_VALUES)) pair else (pair._2, pair._1)

    pure def normalizeCoinAndTick(tokenPair: TokenPair, tick: int): (TokenPair, int) =
        val tokenIn = tokenPair._1
        val tokenOut = tokenPair._2
        if (tokenLT(tokenIn, tokenOut, TOKEN_NUM_VALUES)) 
            ((tokenIn, tokenOut), tick)
        else 
            ((tokenOut, tokenIn), -tick)         
    
    pure def swapInternallyConsistent(msg: PlaceLimitOrderMsg, swapUpdate: SwapUpdateInfo): bool =
        // sanity check that the output from the swap is internally consistent: what user got from pools and tranches
        // should correspond to the outAmount
        val poolsUpdateInfo = swapUpdate.poolUpdates
        val tranchesUpdateInfo = swapUpdate.tranchesUpdates
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
        
        // the tranches and pool updates are in line with what the user received
        userGotFromPools + userGotFromTranches == swapUpdate.outAmount    

    pure def calculateTrancheKey(makerToken: Token, takerToken: Token, tick: TickIndex, tranches: TrancheKey -> Tranche, expirationBlockTime: BlockTime): TrancheKey = 
        val appropriateTrancheKeys = tranches.keys().filter(key =>
            val tranche = tranches.get(key) 
            and{
                key.makerToken == makerToken, 
                key.takerToken == takerToken,
                key.tick == tick, 
                tranche.isFresh,
                tranche.expirationTime == expirationBlockTime,
                tranche.isActive
            }
        )            
        
        if (size(appropriateTrancheKeys) > 0)
            getMinimumTrancheKey(appropriateTrancheKeys)                                        
        else
            val maxTrancheId = tranches.keys().fold(
                0,
                (acc, key) => if (key.id > acc) key.id else acc
            )
            // in the implementation, the id is connected to the block number. Here, we use increasing values
            {makerToken: makerToken, takerToken: takerToken, tick: tick, id: maxTrancheId + 1}
   
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

    pure def getMinimumTrancheKey(trancheKeys: Set[TrancheKey]): TrancheKey =
        trancheKeys.fold(
            // @audit-issue trancheKeys.chooseSome() ideally, but that one is still unimplemented in the simulation
            {makerToken: "_", takerToken: "_", tick: MAX_TICK*10, id: 0},
            (acc, key) => if (trancheLTE(key, acc)) key else acc
        )
    
    pure def getMinimumPoolKey(poolKeys: Set[PoolId], isToken0Liquidity: bool): PoolId =
        // start with a max_tick as initial, so that as soon as there is something found we replace the initial tick
        val initTick = if (isToken0Liquidity) -MAX_TICK else MAX_TICK
        val minKey = poolKeys.fold(
            // ideally, choose some, but doesn't work in the simulation
            {tokenPair: ("_", "_"), tick: initTick, fee: 0},
            (acc, key) => if (poolLTE(key, acc, isToken0Liquidity)) key else acc
        )
        minKey

    // TRADE-RELATED FUNCTIONS
    // this function returns:
    //  - a mapping from poolIds to amounts that should be swapped with this pool
    //  - a mapping from trancheKeys to amounts that should be filled with this tranche
    //  - an integer saying how much of the order amount is still remaining
    pure def swap(
        tokenIn: Token,
        tokenOut: Token,
        tranches: TrancheKey -> Tranche,
        pools: PoolId -> Pool,
        amount: int,
        maxPriceTick: int,
        // TODO: maxAmountOut parameter not taken into account
        maxAmountOut: int 
    ): SwapUpdateInfo =
        
        val inOutPair = (tokenIn, tokenOut)
        // I need this for pools because they are stored only under normalized pairs
        val normalizedPair = orderTokenPair(inOutPair)
        val isPairNormalized = inOutPair == normalizedPair
        val askingForToken0Liquidity = inOutPair._2 == normalizedPair._1
        
        // @audit-info check if key.tick < maxPriceTick is the right semantics, or it should be on average
        // checked: it should be both.
        val relevantTranches = tranches.keys()
            .filter(key => and{
                key.makerToken == tokenOut,
                key.takerToken == tokenIn,
                key.tick <= maxPriceTick,
                tranches.get(key).isActive,
                tranches.get(key).reservesMaker > 0
                })
                        
        val relevantPools = pools.keys()
            .filter(id => and{
                id.tokenPair == normalizedPair,
                computeReserveTick(id.tick, askingForToken0Liquidity, id.fee) <= maxPriceTick
                })
            
        val relevantLiquiditySize = size(relevantTranches) + size(relevantPools)

        // the purpose of this fold is to iterate as many times as there are keys. But the iteration is not
        // happening over keys: instead, a minimum key is found in each iteration and the iteration continues.
        val foldResult = 1.to(relevantLiquiditySize).fold(
            {
                tranchesUpdates: Map(), 
                poolUpdates: Map(), 
                remainingAmountToExchange: amount,                
                remainingTranches: relevantTranches, 
                remainingPools: relevantPools,
                outAmount: 0},
            (acc, _) =>
                // Iterate only if there is still amount to exchange
                if (acc.remainingAmountToExchange == 0) acc else
                
                // find me the minumum tranche key and the minimum pool key
                val minTrancheKey: TrancheKey = getMinimumTrancheKey(acc.remainingTranches)
                val minPoolKey = getMinimumPoolKey(acc.remainingPools, askingForToken0Liquidity)
                
                // the pool price includes the fee
                val minPoolReserveTick = computeReserveTick(minPoolKey.tick, askingForToken0Liquidity, minPoolKey.fee)
                
                // use the tranche key only if there are no more pools  or there are pools, but
                //  their best price is strictly larger than that of the tranches
                if (size(acc.remainingPools) == 0 or minTrancheKey.tick < minPoolReserveTick)                        
                    // I want to know how much I can exchange in this tranche 
                    val tranche = tranches.get(minTrancheKey)
                    val exchangeInfo = calculateTrancheExchangableAmount(acc.remainingAmountToExchange, tranche)
                    // update the acc value
                    {
                        tranchesUpdates: acc.tranchesUpdates.put(
                            minTrancheKey, 
                            {makerReservesDiff: -exchangeInfo.exchangedMakerAmount, takerReservesDiff: exchangeInfo.exchangedTakerAmount}),
                        remainingAmountToExchange: acc.remainingAmountToExchange - exchangeInfo.exchangedTakerAmount,
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
                    val userPutsIntoPool = 
                        if (requiredAmount <= reservesTokenB) acc.remainingAmountToExchange 
                        else intDivDec(reservesTokenB, exchangeRate).truncate()

                    {
                        remainingPools: acc.remainingPools.setRemove(minPoolKey),
                        remainingAmountToExchange: acc.remainingAmountToExchange - userPutsIntoPool,
                        outAmount: acc.outAmount + userGetsFromPool,
                        poolUpdates: acc.poolUpdates.put(
                            minPoolKey, 
                            if (isPairNormalized) {token0Diff: userPutsIntoPool, token1Diff: -userGetsFromPool} 
                            else {token0Diff: -userGetsFromPool, token1Diff: userPutsIntoPool}
                            ),
                        outAmount: acc.outAmount + userGetsFromPool,
                        ...acc
                    }                        
        )
        
        {remainingAmount: foldResult.remainingAmountToExchange, outAmount: foldResult.outAmount, poolUpdates: foldResult.poolUpdates, tranchesUpdates: foldResult.tranchesUpdates}
        

    pure def mayExpire(tranche: Tranche): bool = 
        tranche.expirationTime >= 0

    pure def validatePoolsUpdates(before: State, after: State, poolsUpdates: PoolId -> PoolUpdateDiff): bool = 
        
        // updated pools equal the 'after' state, the rest is unchanged
        before.pools.keys().forall(poolId => 
            if (poolsUpdates.keys().contains(poolId))
                val updatedReserves = (before.pools.get(poolId).reserves._1 + poolsUpdates.get(poolId).token0Diff,
                                        before.pools.get(poolId).reserves._2 + poolsUpdates.get(poolId).token1Diff)
                after.pools.get(poolId).reserves == updatedReserves
            else
                before.pools.get(poolId).reserves == after.pools.get(poolId).reserves
        )

    pure def validateTranchesUpdates(before: State, after: State, tranchesUpdateInfo: TrancheKey -> TrancheUpdateDiff) : bool = 
        
        // for all traded tranches, we check whether the difference matches
        tranchesUpdateInfo.keys().forall(trancheKey =>
            after.tranches.get(trancheKey) ==
            {
                reservesMaker: before.tranches.get(trancheKey).reservesMaker + tranchesUpdateInfo.get(trancheKey).makerReservesDiff,
                reservesTaker: before.tranches.get(trancheKey).reservesTaker + tranchesUpdateInfo.get(trancheKey).takerReservesDiff,
                totalTaker: before.tranches.get(trancheKey).totalTaker + tranchesUpdateInfo.get(trancheKey).takerReservesDiff,
                isFresh: false,
                ...before.tranches.get(trancheKey),
            }
        )            

    pure def updatePools(pools: PoolId -> Pool, poolUpdates: PoolId -> PoolUpdateDiff): PoolId -> Pool =
        pools.keys().mapBy(poolId =>
            if (poolUpdates.keys().contains(poolId))
                {
                    reserves: (pools.get(poolId).reserves._1 + poolUpdates.get(poolId).token0Diff,
                               pools.get(poolId).reserves._2 + poolUpdates.get(poolId).token1Diff),
                    shares: pools.get(poolId).shares
                }
            else
                pools.get(poolId)
        )

    pure def updateTranches(tranches: TrancheKey -> Tranche, tranchesUpdateInfo: TrancheKey -> TrancheUpdateDiff): TrancheKey -> Tranche =
        tranches.keys().mapBy(trancheKey =>
            if (tranchesUpdateInfo.keys().contains(trancheKey))
                {
                    reservesMaker: tranches.get(trancheKey).reservesMaker + tranchesUpdateInfo.get(trancheKey).makerReservesDiff,
                    reservesTaker: tranches.get(trancheKey).reservesTaker + tranchesUpdateInfo.get(trancheKey).takerReservesDiff,
                    totalTaker: tranches.get(trancheKey).totalTaker + tranchesUpdateInfo.get(trancheKey).takerReservesDiff,
                    isFresh: false,
                    ...tranches.get(trancheKey)
                }
            else
                tranches.get(trancheKey)
        )

    pure def tokenInSpentBasedOnOrderType(orderType: LimitOrderType, amountIn: int, remainingAmount: int): int = 
        match orderType {
            | GoodTillCancelled => amountIn
            | FillOrKill => 0
            | ImmediateOrCancel => amountIn - remainingAmount
            // not taking into account if the block is given in the past, assuming the code forbids this
            | GoodTillTime(_) => amountIn
            | JustInTime => amountIn
        }        

    pure def placeRemainingLimitOrderAmount(tranches: TrancheKey->Tranche, shares: Shares, msg: PlaceLimitOrderMsg, amount: int, expirationBlockTime: BlockTime)
    : {tranches: TrancheKey->Tranche, tranchesShares: Shares} =
        
        if (amount == 0) {tranches: tranches, tranchesShares: shares}
        else 
        // now it is reversed: the remaining amount of the tokenIn becomes a maker token of a new tranche
        val makerToken = msg.tokenIn
        // and the tokenOut, the one that the creator wants to receive, becomes the taker token
        val takerToken = msg.tokenOut        
        // this will be the tick of the new tranche, in which we put the tokenIn as the maker token
        val tick = -msg.tick
        val creator = msg.creator
        
        val trancheKey = calculateTrancheKey(makerToken, takerToken, tick, tranches, expirationBlockTime)                      
        val tranchesAndSharesUpdate = 
        if (tranches.keys().contains(trancheKey)) // if the key was one of the previously existing
            val updatedTranches = tranches.setBy(trancheKey, 
                old => {reservesMaker: old.reservesMaker + amount, totalMaker: old.totalMaker + amount, ...old})
            val newShares = if (shares.get(creator).keys().contains(trancheKey))
                {
                    sharesOwned: shares.get(creator).get(trancheKey).sharesOwned + amount, 
                    sharesWithdrawn: shares.get(creator).get(trancheKey).sharesWithdrawn
                } else {sharesOwned: amount, sharesWithdrawn: 0}
            val updatedShares = shares.setBy(
                creator,
                (old => old.put(trancheKey, newShares))
            )
            {tranches: updatedTranches, tranchesShares:updatedShares}
        // if we are adding a fully new tranche
        else                    
            val newTranche = {
                isFresh: true,
                isActive: true,
                reservesMaker: amount,
                reservesTaker: 0,
                totalMaker: amount,
                totalTaker: 0,
                tick: tick,
                expirationTime: expirationBlockTime,
                trancheKey: trancheKey,
                makerDenomination: makerToken,
                takerDenomination: takerToken
            }
            val newShareAccount = {sharesOwned: amount, sharesWithdrawn: 0}                    
            val updatedTranches = tranches.put(trancheKey, newTranche)
            val updatedShares = shares.setBy(
                    creator,
                    old => old.put(trancheKey, newShareAccount)
                )
            {tranches: updatedTranches, tranchesShares: updatedShares}

        tranchesAndSharesUpdate

    // DEPOSIT RELATED FUNCTIONS

    val emptyPool = {reserves: (0, 0), shares: 0}

    // Calculates the amounts that may be deposited so that the pool ratios get preserved
    pure def computeAmountsDeposit(reserves: (int, int), amounts: (int, int)): (int, int) = {
        val inAmount0 = 
            if (reserves._2 > 0) min(amounts._1, fromRatio(reserves._1, reserves._2).mulInt(amounts._2).truncate())
            else amounts._1
        val inAmount1 = 
            if (reserves._1 > 0) min(amounts._2, fromRatio(reserves._2, reserves._1).mulInt(amounts._1).truncate())
            else amounts._2
        
        (inAmount0, inAmount1)
    }

    pure def reservesRatio(p: Pool): Dec = 
        // assumes that reserves._2 is positive
        fromRatio(p.reserves._1, p.reserves._2)

    // LP.2 in specification invariants
    // autoswapFee = residualValue0 * (1 - 1.0001^-fee) + residualValue1 * (1 - 1.0001^-fee) * price1To0
    pure def computeAutoswapFee(residualAmounts: (int, int), tick0To1: TickIndex, fee: TickIndex): Dec =
        val res0 : Dec = residualAmounts._1.fromInt()
        val res1 : Dec = residualAmounts._2.fromInt()
        val factor: Dec = one.sub(priceFromTick(fee))
        val price1To0: Dec = priceFromTick(-tick0To1)
        add(
            res0.mul(factor),
            res1.mul(factor).mul(price1To0)
        )        
        
    // @audit-info not sure if to compute depositValue when autoswapping I should use the full amount in the
    // deposit message or just the one computed by computeAmountsDeposit
    // can totalPoolValue be 0? Invariant candidate!
    // 
    // LP.1 and LP.2 in specification invariants
    // shareValue = totalPreexistingShares / (totalPoolValue + autswapFee)
    // sharesMinted = (depositValue - autoswapFee) * shareValue
    pure def computeSharesDeposit(existingShares: int, reserves: (int, int), amounts: (int, int), tick: int, autoswapFee: Dec): int =
        
        val poolValue = computePoolValue(reserves, tick)
        val depositValue: Dec = computeDepositValue(amounts, tick)      
        if (poolValue.equal(zero)) depositValue.truncate()
        else 
            val shareValue = intDivDec(existingShares, poolValue.add(autoswapFee))
            sub(depositValue, autoswapFee).mul(shareValue).truncate()

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

    // Assembles a set of tuples of creator and trancheKey, where the creator has some ownership in the tranche
    // Useful when nondeterministically selecting a creator and a tranche inside withdraw or cancel action
    pure def nontrivialTranchesOwners(tranchesShares: Shares, users: Set[Addr]): Set[(Addr, TrancheKey)] = 
        users.fold(
            Set(),
            (acc, user) => val newTuples = 
                // for each creator's shares
                tranchesShares.getOrElse(user, Map()).keys()
                    // take into account those shares in which the creator has some ownership
                    .filter(trancheKey => tranchesShares.get(user).get(trancheKey).sharesOwned > 0)
                    // and then create a tuple of creator and trancheKey
                    .map(trancheKey => (user, trancheKey))
                acc.union(newTuples)
        )    
    
    pure def computeWithdrawAmountTakerToken(tranche: Tranche, userShares: UsersTrancheShares): {taker_token: int, shares: int} = 

        val percentFilled = calculatePercentFilled(tranche)
        val withdrawAmountInMakerToken = percentFilled.mulInt(userShares.sharesOwned).truncate() - userShares.sharesWithdrawn                
        val withdrawAmountInTakerToken = intDivDec(withdrawAmountInMakerToken, priceFromTick(tranche.tick)).truncate()

        {taker_token: withdrawAmountInTakerToken, shares: withdrawAmountInMakerToken}
    
    // A function to compute the amount in a different way, corresponding to MLO 1.e
    // https://www.notion.so/hadron/Dex-Formal-Verification-Spec-c13256ab54cd4d82a6ab09a88863e52b?pvs=4#95dd3f39f49c4b0c928fc14a2e7ec303
    // pure def computeWithdrawAmountTakerTokenCheck(tranche: Tranche, userShares: UsersTrancheShares): {taker_token: int, shares: int} = 
    //     val proRataShare = fromRatio(userShares.sharesOwned, tranche.totalMaker)
    //     val priceTakerToMaker = priceFromTick(tranche.tick)
    // WITHDRAW POOL RELATED FUNCTIONS

    // Assumes that totalShares > 0, which is checked by the invariants
    // LP.4 in specification invariants
    pure def computeAmountsWithdrawnPool(reserves: (int, int), totalShares: int, sharesToRemove: int): (int, int) =

        // We mimic the implementation here for capturing truncation. The spec gives a more intuitive expression,
        // proRataShare = sharesToRemove / totalShares, which is then multiplies with total reserves.
        val outAmount0 = fromRatio(reserves._1*sharesToRemove, totalShares).truncate()
        val outAmount1 = fromRatio(reserves._2*sharesToRemove, totalShares).truncate()
        (outAmount0, outAmount1)

    pure def bothSides(p: Pool): bool = 
        p.reserves._1 > 0 and p.reserves._2 > 0

    pure def getPoolId(tokens: (Token, Token), tick: TickIndex, fee: Fee): PoolId = 
      {tokenPair: tokens, tick: tick, fee: fee}

    pure def trancheLTE(t1: TrancheKey, t2: TrancheKey): bool = 
        // assume here: t1.pair == t2.pair; otherwise, unordered
        if (t1.tick == t2.tick)
            t1.id <= t2.id
        else
            t1.tick <= t2.tick

    pure def poolLTE(p1: PoolId, p2: PoolId, isToken0Liquidity: bool): bool =
        val tick1 = computeReserveTick(p1.tick, isToken0Liquidity, p1.fee)
        val tick2 = computeReserveTick(p2.tick, isToken0Liquidity, p2.fee)
        // if the ticks are the same
        if (tick1 == tick2)
            // then order by fee
            p1.fee <= p2.fee
        else
            // otherwise, order by ticks
            tick1 <= tick2        

    pure def computeReserveTick(poolTick: TickIndex, isToken0Liquidity: bool, fee: Fee): TickIndex = 
        if (isToken0Liquidity) 
            -poolTick + fee 
        else 
            poolTick + fee

    pure def updateSwapsBool(user: Addr, poolIds: Set[PoolId], userDeposits: PoolId -> {amounts: (int, int), swaps: bool}, userShares: PoolId -> int):
    PoolId -> {amounts: (int, int), swaps: bool} = 
        
        poolIds.fold(userDeposits, (acc, id) => 
            if (userDeposits.has(id) and userShares.get(id) > 0)
                acc.set(id, {swaps: true, ...userDeposits.get(id)})
            else acc
            )

    // CANCEL RELATED FUNCTIONS
    pure def computeCancelAmountMakerToken(tranche: Tranche, usersShares: UsersTrancheShares): int =     
        // @audit-info The calculation here is different (but equivalent) than in the spec. 
        // Are there perhaps some reasons to stick to the spec calculation?   
        val percentUnfilled = calculatePercentUnfilled(tranche)
        val sharesOwned = usersShares.sharesOwned
        percentUnfilled.mulInt(sharesOwned).truncate()    


}
