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
    
    var state: State
    var postViolation: bool

    pure def ok(state: State): Result = {state: state, result: "nil"}

    pure def error(state: State, error: str): Result = {state: state, result: error}

    pure def computeAmountFromPrice(usedAmount: int, price: Dec): int

    pure def computeAmountsDeposit(reserves: (int, int), amounts: (int, int)): (int, int) = amounts

    pure def computeSharesDeposit(existingShares: int, amounts: (int, int)): (int, int) = amounts

    pure def computeAmountsWithdraw(reserves: (int, int), totalShares: int, shares: int): (int, int) = reserves

    pure def getPoolId(tokens: (Token, Token), tick: TickIndex, fee: Fee): PoolId = 
      {tokenPair: tokens, tick: tick, fee: fee}

    pure def orderTokenPair(pair: (Token, Token)): (Token, Token) = pair

    pure def deposit(state: State, msg: DepositMsg) : Result = {
      ok(state)
    }

    pure def postDeposit(before: State, after: State, msg: DepositMsg): bool = 
      // We could guarantee by design that the token pair is submitted in order
      val pair =  orderTokenPair(msg.tokenPair)
      val amounts = if (pair == msg.tokenPair) msg.amountPair else (msg.amountPair._2, msg.amountPair._1)
      val poolId = getPoolId(pair, msg.tick, msg.fee)
      // ** State changes validation **
      // Pool exists
      after.pools.has(poolId) and 
      val reservesBefore = if (before.pools.has(poolId)) (0,0) else before.pools.get(poolId).reserves
      val reservesAfter = after.pools.get(poolId).reserves
      val sharesBefore = if (before.pools.has(poolId)) 0 else before.pools.get(poolId).shares
      val sharesAfter = after.pools.get(poolId).shares
       //@audit-info Why one may not be allowed to deposit the full amount? ratios?
      val inAmounts = computeAmountsDeposit(reservesBefore, amounts)
      val outShares = computeSharesDeposit(sharesBefore.shares, inAmounts)
      // Reserves are updated
      reservesAfter._1 == reservesBefore._1 + inAmounts._1 and 
      reservesAfter._2 == reservesBefore._2 + inAmounts._2 and
      // Coins are transferred
      after.coins.get(msg.creator).get(pair._1) >= 0 and after.coins.get(msg.creator).get(pair._2) >= 0 and
      after.coins.get(msg.creator).get(pair._1) == before.coins.get(msg.creator).get(pair._1) - inAmounts._1 and
      after.coins.get(msg.creator).get(pair._2) == before.coins.get(msg.creator).get(pair._2) - inAmounts._2 and
      // Shares are minted
      after.sharesPrime.get(msg.receiver).has(poolId) and
      after.sharesPrime.get(msg.receiver).get(poolId) == if (before.sharesPrime.get(msg.receiver).has(poolId)) before.sharesPrime.get(msg.receiver).get(poolId) + outShares
                                                         else outShares and
      sharesAfter == sharesBefore + outShares

    pure def withdraw(state: State, msg: WithdrawMsg) : Result = {
      ok(state)
    }
    
    pure def postWithdraw(before: State, after: State, msg: DepositMsg): bool = 
      val pair =  orderTokenPair(msg.tokenPair)
      val poolId = getPoolId(pair, msg.tick, msg.fee)
      // We assume that we do not remove empty pools
      // We do not check that poolBefore exists, as it is a precondition and not a postcondition
      val reservesBefore = before.pools.get(poolId).reserves
      val reservesAfter = after.pools.get(poolId).reserves
      val sharesBefore = before.pools.get(poolId).shares
      val sharesAfter = after.pools.get(poolId).shares
      val outAmounts = computeAmountsWithdraw(reservesBefore, sharesBefore, msg.amount)
      // ** State changes validation **
      // Reserves are updated
      reservesAfter._1 == reservesBefore._1 - outAmounts._1 and reservesAfter._1 >= 0 and
      reservesAfter._2 == reservesBefore._2 - outAmounts._2 and reservesAfter._2 >= 0 and
      // Coins are transferred
      after.coins.get(msg.receiver).get(pair._1) == before.coins.get(msg.receiver).get(pair._1) + outAmounts._1 and
      after.coins.get(msg.receiver).get(pair._2) == before.coins.get(msg.receiver).get(pair._2) + outAmounts._2 and
      // Shares are burned
      // We assume that we do not remove empty shares
      after.sharesPrime.get(msg.creator).get(poolId) >= 0 and
      after.sharesPrime.get(msg.creator).get(poolId) == before.sharesPrime.get(msg.creator).has(poolId) - msg.amount and
      sharesAfter >= 0 and sharesAfter == sharesBefore - msg.amount

    pure def swap(state: State, msg: SwapMsg) : Result = {
      ok(state)
    }

    /* pure def minPricePool(ids: Set[PoolId], AtoB: bool): PoolId = {
      ids.fold(MAX_PICE_PRICE, (acc, id) => val price = computePrice(id.tick, id.fee)
                                            take the minimum)
      ids.chooseSome
    }

    computePrice(): int = val realTick = if (AtoB) tick + fee else - tick - fee
                          fromTickToPrice(realTick)

    pure def computeTick(tick: TickIndex, AtoB: bool, fee: Fee): TickIndex = {
      tick
    } */

    pure def liquidityIteration(pair: (Token, Token), token: Token, pools: PoolId -> Pool, amount: int): PoolId -> (int, int) = Map()
      /* val AtoB = pair._1 == token
      val relevantPools = pools.keys().filter(id => id.tokenPair == pair)
      relevantPools.fold({remainingPools: relevantPools, inAmount: amount, outAmount: 0, result: relevantPools.mapBy(pool => (0, 0))}, (acc, id) =>
        val min =  minPricePool(acc.remainingPools, AtoB)
        val tick = computeTick(min.tick, AtoB, min.fee)
        val price = priceFromTick(tick)
        val outAmount = 1
        {remainingPools: acc.remainingPools, inAmount: 0, outAmount: 0, result: acc.result}).result */

    pure def postconditionSwapPool(poolBefore: State, poolAfter: State, poolPair: (Token, Token), msgPair: (Token, Token), amounts: (int, int)): bool =      
      if (poolPair._1 == msgPair._1) poolAfter.reserves._1 == poolBefore.reserves._1 - amounts._1 and
                                     poolAfter.reserves._2 == poolBefore.reserves._2 + amounts._2      
      else poolAfter.reserves._1 == poolBefore.reserves._1 + amounts._1 and 
           poolAfter.reserves._1 == poolBefore.reserves._2 - amounts._2

    pure def postSwap(before: State, after: State, msg: SwapMsg): bool =
      // Assumes that if we check the postcondition is because the action succeeded: we managed to swap to full amount
      val inAmount = msg.amount
      val pair =  orderTokenPair(msg.tokenPair)
      val resultIteration = liquidityIteration(pair, msg.token, before.pools, inAmount)
      val outAmount = if (pair._1 == msg.token) resultIteration.keys().fold(0, (acc, id) => acc + resultIteration.get(id)._2)
                      else resultIteration.keys().fold(0, (acc, id) => acc + resultIteration.get(id)._1)
      val usedAmount = if (pair._1 == msg.token) resultIteration.keys().fold(0, (acc, id) => acc + resultIteration.get(id)._1)
                       else resultIteration.keys().fold(0, (acc, id) => acc + resultIteration.get(id)._2)
      // ** State changes validation **
      // Coins are transferred
      after.coins.get(msg.creator).get(msg.tokenPair._1) == before.coins.get(msg.creator).get(msg.tokenPair._1) - usedAmount and
      after.coins.get(msg.receiver).get(msg.tokenPair._2) == before.coins.get(msg.receiver).get(msg.tokenPair._2) + outAmount and
      // Reserves are updated
      resultIteration.keys().forall(id => val poolBefore = before.pools.get(id)
                                          val poolAfter = after.pools.get(id)
                                          postconditionSwapPool(poolBefore, poolAfter, pair, msg.tokenPair, resultIteration.get(id))) and
      // ** Specification invariants **
      // Invariant Swap.1a
      val minAmount = computeAmountFromPrice(usedAmount, msg.limitPrice)
      outAmount >= minAmount and
      // Invariant Swap.1b
      inAmount >= usedAmount

    pure def placeLimitOrder(state: State, msg: PlaceLimitOrderMsg) : State =
        val normalizedCoinAndTick = normalizeCoinAndTick(msg.tokenPair, msg.tick)        
        val tick1to0 = normalizedCoinAndTick._3
        val price1To0 = priceFromTick(tick1to0)
        // TODO: unfinished
        state


    pure def cancelLimitOrder(state: State, msg: CancelLimitOrderMsg) : State =
        // TODO: unfinished
        state 


    pure def withdrawLimitOrder(state: State, msg: WithdrawLimitOrderMsg) : State =
        // TODO: unfinished
        state   

    // Invariant: postconditions always hold
    val postconditions = postViolation

    // Invariant: pool reserves are always greater or equal to zero 
    val nonNegativeReserves = state.pools.keys().forall(poolId => val reserves = state.pools.get(poolId).reserves
                                                                  reserves._1 >= 0 and reserves._2 >= 0)

    // Invariant: shares supplies are always greater or equal to zero 
    val nonNegativeSharesSupply = state.pools.keys().forall(poolId => state.pools.get(poolId).shares >= 0)

    // Invariant: coin and share balances are zero or greater 
    val nonNegativeBalances = CREATORS.forall(creator => val coins = state.coins.get(creator)
                                                         val shares = state.sharesPrime.get(creator)
                                                         coins.keys().forall(token => coins.get(token) >= 0) and
                                                         shares.keys().forall(share => shares.get(share) >= 0))
    
    action actionDeposit(msg: DepositMsg): bool = {
      val result = deposit(state, msg)
      all {
        require(result.error=="nil"),
        state' = result.state,
        postViolation' = postDeposit(state, result.state, msg)
      }
    }

    action actionWithdraw(msg: DepositMsg): bool = {
      val result = withdraw(state, msg)
      all {
        require(result.error=="nil"),
        state' = result.state,
        postViolation' = postWithdraw(state, result.state, msg)
      }
    }

    action actionSwap(msg: SwapMsg): bool = {
      val result = swap(state, msg)
      all {
        require(result.error=="nil"),
        state' = result.state,
        postViolation' = postSwap(state, result.state, msg)
      }
    }

    action step: bool = any {
        nondet c = oneOf(CREATORS)
        nondet o = oneOf(ORDER_TYPES)
        nondet t = oneOf(TICKS)
        nondet token_pair = oneOf(TOKEN_PAIRS)        
        val limitOrderMsg = {
            creator: c,
            orderType: o,
            tick: t,
            tokenPair: token_pair
        }
        state' = placeLimitOrder(state, limitOrderMsg),

        nondet creator = oneOf(state.shares.keys())
        nondet trancheKey = oneOf(state.shares.get(creator).keys())
        val cancelLimitOrderMsg = {
            creator: creator,
            trancheKey: trancheKey
        }
        state' = cancelLimitOrder(state, cancelLimitOrderMsg),

        nondet creator = oneOf(state.shares.keys())
        nondet trancheKey = oneOf(state.shares.get(creator).keys())
        val withdrawLimitOrderMsg = {
            creator: creator,
            trancheKey: trancheKey
        }
        all {
            require(state.tranches.keys().contains(trancheKey)),
            require(not(state.tranches.get(trancheKey).isFresh)),
            state' = withdrawLimitOrder(state, withdrawLimitOrderMsg)
        }

    }

  }