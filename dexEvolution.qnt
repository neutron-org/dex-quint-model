// -*- mode: Bluespec; -*-
module dexEvolution {
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"
    import utils.* from "utils"
    import basicSpells.* from "lib/basicSpells"
    import Dex.* from "dex"
    import postconditions.* from "postconditions"

    // STATE VARIABLES
    var state: State  
    var postconditionOK: bool
    var actionSuccess: bool
    var actionTaken: Transition

    // STATE MACHINE
    action init = all {
        // @audit-issue This should be the correct spec, but replaced by a simpler, hard-coded one due to
        // a problem in the simulator: https://github.com/informalsystems/quint/issues/1530
        // val possibleTokenAccounts = setOfMaps(CREATORS, setOfMaps(TOKENS, INITIAL_CREATORS_BALANCES))
        // nondet initCoins = oneOf(possibleTokenAccounts)
        val bal = INITIAL_CREATOR_BALANCE_TEMP
        val initCoins = CREATORS.fold(Map(), (acc, c) => acc.put(c, Map("A" -> bal, "B" -> bal)))        
        state' = initState(initCoins),
        postconditionOK' = true,
        actionTaken' = {name: "init", message: Init},
        actionSuccess' = true
    }

    action step: bool = any {
        placeLimitOrderAction,
        cancelLimitOrderAction,
        withdrawLimitOrderAction,
        advanceTimeAction,
        withdrawPoolAction,
        depositAction,
        singlehopSwapAction
    }

    

    // ACTIONS

    action withdrawPoolAction: bool = all {
        // mapping from creators to sets of pools in which they have >0 share
        val relevantCreatorsAndPools = state.poolShares.keys().fold(
            Map(), // a map from creators to pools in which they have non-trivial stake
            (acc, c) => 
            val nonTrivialPools = state.poolShares.get(c).keys().filter(p => state.poolShares.get(c).get(p) > 0)
            if (nonTrivialPools.size() > 0)
                acc.put(c, nonTrivialPools)
            else
                acc
        )
        all {
            require(relevantCreatorsAndPools.keys().size() > 0),        
            nondet creator = oneOf(relevantCreatorsAndPools.keys())
            nondet poolKey = oneOf(relevantCreatorsAndPools.get(creator))
            nondet amountToWithdraw = oneOf(1.to(state.poolShares.get(creator).get(poolKey)))
            // @audit-info Incorporate in the evolution
            val msg = {
                creator: creator,
                tokenPair: poolKey.tokenPair,
                amount: amountToWithdraw,
                tick: poolKey.tick,
                fee: poolKey.fee
            }
            val result = withdrawPool(state, msg)
            all {        
                actionSuccess' = result.error == "nil",
                state' = result.state,
                postconditionOK' = withdrawPoolPostcondition(state, result.state, msg),
                actionTaken' = {name: "withdraw", message: WithdrawPoolSubType(msg)}
            }
        }
    }
        

    action depositAction: bool = all {
        require(state.pools.keys().size() > 0),
        nondet doAutoswap = oneOf(Set(true, false))
        // a set of {creator: Addr, poolKey: PoolKey} records containing all the pools whose tokens are 
        // the ones in which the creator has >0 coins. We do so by iterating first over creators, and then
        // over pools, and checking if the creator has >0 coins in at least one of pool tokens.
        val creatorsAndPools = CREATORS.fold(
            Set(),  
            (acc, c) => acc.union(
                state.pools.keys().fold(
                    Set(),
                    (acc2, poolKey) => 
                    if (state.coins.get(c).get(poolKey.tokenPair._1) > 0 or state.coins.get(c).get(poolKey.tokenPair._2) > 0)
                        acc2.union(Set({creator: c, poolKey: poolKey}))
                    else
                        acc2
                )
            )
        )
        nondet creatorAndPool = oneOf(creatorsAndPools)
        val poolKey = creatorAndPool.poolKey        
        val c = creatorAndPool.creator
        nondet amountToken1 = oneOf(0.to(state.coins.get(c).get(poolKey.tokenPair._1)))
        nondet amountToken2 = oneOf(0.to(state.coins.get(c).get(poolKey.tokenPair._2)))
        
        val depositAmounts = computeAmountsDeposit(state.pools.get(poolKey).reserves, (amountToken1, amountToken2))
        
        val msg: DepositMsg = {
            creator: c,
            tokenPair: poolKey.tokenPair,
            amountPair: depositAmounts,
            fee: poolKey.fee,
            tick: poolKey.tick,
            autoswap: doAutoswap
        }
        val result = deposit(state, msg)
        all {
            require(depositAmounts._1 > 0 or depositAmounts._2 > 0),
            actionSuccess' = result.error == "nil",
            state' = result.state,
            postconditionOK' = depositPostcondition(state, result.state, msg),
            actionTaken' = {name: "deposit", message: DepositPoolSubType(msg)}
        }
    }


    action placeLimitOrderAction: bool = 
        nondet c = oneOf(CREATORS)
        nondet o_init = oneOf(ORDER_TYPES)
        nondet block_delta = oneOf(1.to(MAX_FUTURE_BLOCKS))
        val o = if (o_init == GoodTillTime(-1)) GoodTillTime(state.blockNumber + block_delta) else o_init

        nondet t = oneOf(TICKS)
        nondet tokenIn = oneOf(TOKENS)
        nondet tokenOut = oneOf(TOKENS.filter(t => t != tokenIn)) 

        // @audit-info We allow here to select 0 (so as to avoid oneOf on an empty set), but 0 value is disallowed
        // in the requirement below
        val possibleAmounts = 0.to(state.coins.get(c).get(tokenIn)) 
        nondet amount = oneOf(possibleAmounts)
        // @audit-info not implemented for now, so using a trivial value
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
            require(limitOrderMsg.amountIn > 0),
            state' = res.state,
            // @audit-info TODO: this check is until Postcondition is updated to support different types
            postconditionOK' = 
            if (limitOrderMsg.orderType == GoodTillCancelled ) 
                placeLimitOrderPostcondition(state, limitOrderMsg, res.state)
            else true,
            actionTaken' = {name: "placeLimitOrder", message: PlaceLimitOrderSubType(limitOrderMsg)},
            actionSuccess' = res.error == "nil"
        }     


    
    
    action singlehopSwapAction: bool = all {
        require(state.pools.keys().size() > 0),
        // we will model swapping only the token pairs for which pools already exist
        nondet existingPair = state.pools.keys().map(k => k.tokenPair).oneOf()
        // becuase pools are indexed by sorted pairs, we need to be able to create the opposite pair, too
        nondet switchPair = oneOf(Set(true, false))
        val pair = if (switchPair) (existingPair._2, existingPair._1) else existingPair
        nondet creator = oneOf(CREATORS)
        nondet amount = 1.to(MAX_AMOUNT).oneOf()
        nondet tick = oneOf(TICKS)
        val msg = {
            creator: creator,
            tokenPair: pair,
            amount: amount,
            limitPriceTick: tick
        }
        val result = singlehopSwap(state, msg)
        all {
            actionSuccess' = result.error=="nil",
            state' = result.state,
            postconditionOK' = singlehopSwapPostcondition(state, result.state, msg),
            actionTaken' = {name: "swap", message: SwapSubType(msg)}
        }
    }


action withdrawLimitOrderAction: bool = 
    val creatorKeyTuples = nontrivialTranchesOwners(state.tranchesShares, CREATORS)
    all {
        require(size(creatorKeyTuples) > 0),
        nondet chosenTuple = oneOf(creatorKeyTuples)
        val creator = chosenTuple._1
        val trancheKey = chosenTuple._2
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
    }

    

    action cancelLimitOrderAction: bool = 
    // only those addresses that have some tranches can cancel them
        val creatorKeyTuples = nontrivialTranchesOwners(state.tranchesShares, CREATORS)
        all{        
            require(size(creatorKeyTuples) > 0),
            // take one of the creators that have some tranches
            nondet chosenTuple = oneOf(creatorKeyTuples)
            val creator = chosenTuple._1
            val trancheKey = chosenTuple._2
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
    }

    action advanceSingleBlockAction: bool =
        state' = advanceBlocks(state, 1).state

    action advanceMultipleBlocksAction: bool =
        nondet timestep = oneOf(1.to(MAX_FUTURE_BLOCKS))
        state' = advanceBlocks(state, timestep).state

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



// INVARIANTS
    // Invariant: postconditions always hold
    val postconditions = postconditionOK

    // Invariant: pool reserves are always greater or equal to zero 
    val nonNegativeReserves = state.pools.keys().forall(poolId => val reserves = state.pools.get(poolId).reserves
                                                                  reserves._1 >= 0 and reserves._2 >= 0)

    // Invariant: shares supplies are always greater or equal to zero 
    val nonNegativeSharesSupply = state.pools.keys().forall(poolId => state.pools.get(poolId).shares >= 0)

    // Invariant: coin and share balances are zero or greater 
    val nonNegativeBalances = CREATORS.forall(
        creator => val coins = state.coins.getOrElse(creator, Map())
                   val shares = state.poolShares.getOrElse(creator, Map())
                   coins.keys().forall(token => coins.get(token) >= 0) and
                   shares.keys().forall(share => shares.get(share) >= 0))
    
    // Invariant: LP profit from their deposits
    // LP.5 in specification invariants
    val profitDeposit =
      CREATORS.forall(
        creator => state.deposits.get(creator).keys().forall(
            poolId => if (state.poolShares.get(creator).get(poolId) == 0)
                        val deposit = state.deposits.get(creator).get(poolId)
                        val withdrawnAmounts = state.poolWithdraws.get(creator).get(poolId)
                        if (deposit.swaps) withdrawnAmounts._1 > deposit.amounts._1 and withdrawnAmounts._2 > deposit.amounts._2
                        else withdrawnAmounts._1 == deposit.amounts._1 and withdrawnAmounts._2 == deposit.amounts._2
                      else true))

    // Invariant: Tranche expiration times always >= -1 (sanity check)
    val expirationTimeSanity = 
        state.tranches.keys().forall(k => state.tranches.get(k).expirationTime >= -1)
    }