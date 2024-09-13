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

    pure def computeAmountsDeposit(reserves: (int, int), amounts: (int, int)): (int, int) = amounts

    pure def computeSharesDeposit(existingShares: int, amounts: (int, int)): (int, int) = amounts

    pure def getPoolId(tokens: (Token, Token), tick: int, fee: int): PoolId = 
      {tokenPair: tokens, tick: tick, fee: fee}

    pure def orderTokenPair(pair: (Token, Token)): (Token, Token) = pair

    pure def deposit(state: State, msg: DepositMsg) : Result = {
      ok(state)
    }

    pure def postDeposit(before: State, after: State, msg: DepositMsg): bool = val pair =  orderTokenPair(msg.tokenPair)
                                                                               val amounts = if (pair == msg.tokenPair) msg.amountPair else (msg.amountPair._2, msg.amountPair._1)
                                                                               val poolId = getPoolId(pair, msg.tick, msg.fee)
                                                                               after.pools.has(poolId) and 
                                                                               val poolBefore = before.pools.get(poolId)
                                                                               val poolAfter = after.pools.get(poolId)
                                                                               val inAmounts = computeAmountsDeposit(before.reserves, amounts)
                                                                               val outShares = computeSharesDeposit(before.shares, inAmounts)
                                                                               poolAfter.reserves._1 == poolBefore.reserves._1 + inAmounts._1 and 
                                                                               poolAfter.reserves._2 == poolBefore.reserves._2 + inAmounts._2 and
                                                                               after.sharesPrime.get(msg.receiver).has(poolId) and
                                                                               after.sharesPrime.get(msg.receiver).get(poolId) == before.sharesPrime.get(msg.receiver).get(poolId) + outShares and
                                                                               poolAfter.shares == poolBefore.shares + outShares

    pure def withdraw(state: State, msg: WithdrawMsg) : Result = {
      ok(state)
    }
    
    pure def postWithdraw(before: State, after: State, msg: DepositMsg): bool = true

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

    val postconditions = postViolation
    
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