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
        state' = initState,
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
        nondet poolKey = oneOf(state.pools.keys())
        nondet doAutoswap = oneOf(Set(true, false))
        nondet maxAmount = oneOf(1.to(MAX_AMOUNT))
        val depositAmounts = computeAmountsDeposit(state.pools.get(poolKey).reserves, (maxAmount, maxAmount))
        nondet c = oneOf(CREATORS)
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
    val creatorsWithTrancheKeys = CREATORS.fold(
        Map(),
        (acc, creator) => 
            if (state.tranchesShares.keys().contains(creator) and state.tranchesShares.get(creator).keys().size() > 0)
                acc.put(creator, state.tranchesShares.get(creator).keys())
            else
                acc
    )
    all {
        require(size(creatorsWithTrancheKeys.keys()) > 0),
        nondet creator = oneOf(creatorsWithTrancheKeys.keys())
        nondet trancheKey = oneOf(creatorsWithTrancheKeys.get(creator))
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
        val nontrivialTranchesOwners = state.tranchesShares.keys().filter(
            creator => 
            state.tranchesShares.get(creator).keys().size() > 0
            and 
            state.tranchesShares.get(creator).keys().exists(
                trancheKey => 
                state.tranchesShares.get(creator).get(trancheKey).sharesOwned > 0
                )            
            )
        all{        
            require(nontrivialTranchesOwners.size() > 0),
            // take one of the creators that have some tranches
            nondet creator = oneOf(nontrivialTranchesOwners)
            nondet trancheKey = state.tranchesShares.get(creator).keys().filter(
                trancheKey => state.tranchesShares.get(creator).get(trancheKey).sharesOwned > 0
                ).oneOf()
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


    }