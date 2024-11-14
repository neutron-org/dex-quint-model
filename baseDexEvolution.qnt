// -*- mode: Bluespec; -*-
module baseDexEvolution {
    import dec.* from "lib/dec"
    import types.* from "types"
    // import consts.* from "consts"\
    import restrictedConsts.* from "restrictedConsts"
    import utils.* from "utils"
    import basicSpells.* from "lib/basicSpells"
    import Dex.* from "dex"
    import postconditions.* from "postconditions"

    // BASE STATE VARIABLES
    var state: State

    // STATE MACHINE
    action init = all {
        state' = initState,
    }

    action step: bool = any {
        placeLimitOrderAction(assignResult),
        cancelLimitOrderAction(assignResult),
        withdrawLimitOrderAction(assignResult),
        withdrawPoolAction(assignResult),
        depositAction(assignResult),
        singlehopSwapAction(assignResult),
        advanceTimeAction(assignResult),
    }

    action assignResult(result: Result, msg: Message): bool =
        state' = result.state

    // ACTIONS
    action withdrawPoolAction(processResult: (Result, Message) => bool): bool = all {
        // tuples of from creators and pools in which they have >0 share
        val relevantCreatorsAndPools = tuples(CREATORS, state.pools.keys()).filter(((c, p)) => state.poolShares.get(c).getOrElse(p, 0) > 0)
        all {
            require(relevantCreatorsAndPools != Set()),
            nondet creatorAndPool = relevantCreatorsAndPools.oneOf()
            val creator = creatorAndPool._1
            val poolKey = creatorAndPool._2
            nondet amountToWithdraw = oneOf(1.to(state.poolShares.get(creator).get(poolKey)))
            val msg = {
                creator: creator,
                tokenPair: poolKey.tokenPair,
                sharesAmount: amountToWithdraw,
                tick: poolKey.tick,
                fee: poolKey.fee
            }
            val result = withdrawPool(state, msg)
            processResult(result, WithdrawPool(msg))
        }
    }


    action depositAction(processResult: (Result, Message) => bool): bool =
        nondet doAutoswap = oneOf(Set(true, false))
        // a set of (Addr, PoolId) tuples containing all the pools whose tokens are
        // the ones of which the creator has >0 coins.
        // (Otherwise, the creator cannot deposit since it has no coins to deposit)
        val creatorsAndPairs = tuples(CREATORS, TOKEN_PAIRS).filter(((c, tp)) => {
          state.coins.get(c).get(tp._1) > 0 or state.coins.get(c).get(tp._2) > 0
        })
        nondet creatorAndPair = oneOf(creatorsAndPairs)
        val tokenPair = creatorAndPair._2
        val c = creatorAndPair._1
        nondet fee = oneOf(FEES)
        nondet tick = oneOf(TICKS)
        nondet amountTokenX = oneOf(0.to(state.coins.get(c).get(tokenPair._1)))
        nondet amountTokenY = oneOf(0.to(state.coins.get(c).get(tokenPair._2)))
        // we want to let simulation more often go with single-sided deposits (because they are more common, and more likely not to
        // fail due to being behind enemy lines (BEL)
        nondet providedAmounts = Set(
          (amountTokenX, amountTokenY),
          (amountTokenX, 0),
          (0, amountTokenY)
        ).oneOf()

        val msg: DepositMsg = {
            creator: c,
            tokenPair: tokenPair,
            amountPair: providedAmounts,
            fee: fee,
            token0SellTick: tick,
            autoswap: doAutoswap
        }

        all {
            require(providedAmounts._1 > 0 or providedAmounts._2 > 0),
            val result = deposit(state, msg)
            processResult(result, DepositPool(msg))
        }

    action placeLimitOrderAction(processResult: (Result, Message) => bool): bool =
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
        val msg: PlaceLimitOrderMsg = {
            creator: c,
            orderType: o,
            sellTick: t,
            tokenIn: tokenIn,
            tokenOut: tokenOut,
            amountIn: amount,
            maxAmountOut: maxAmountOut
        }

        all {
            require(msg.amountIn > 0),
            val result = placeLimitOrder(state, msg)
            processResult(result, PlaceLimitOrder(msg))            
        }

    action singlehopSwapAction(processResult: (Result, Message) => bool): bool = all {
        require(state.pools.keys().size() > 0),
        // we will model swapping only the token pairs for which pools already exist
        nondet existingPair = state.pools.keys().map(k => k.tokenPair).oneOf()
        // becuase pools are indexed by sorted pairs, we need to be able to create the opposite pair, too
        nondet switchPair = oneOf(Set(true, false))
        val pair = if (switchPair) (existingPair._2, existingPair._1) else existingPair
        nondet creator = oneOf(CREATORS)

        val inTokenTotalAmount = state.coins.get(creator).get(pair._1)
        nondet amount = 0.to(inTokenTotalAmount).oneOf()
        nondet tick = oneOf(TICKS)
        val msg = {
            creator: creator,
            tokenPair: pair,
            amount: amount,
            limitPriceTick: tick
        }
        val result = singlehopSwap(state, msg)
        processResult(result, Swap(msg))
    }

    action withdrawLimitOrderAction(processResult: (Result, Message) => bool): bool =
        val creatorKeyTuples = nontrivialTranchesOwners(state.tranchesShares, CREATORS)
        all {
            require(size(creatorKeyTuples) > 0),
            nondet chosenTuple = oneOf(creatorKeyTuples)
            val creator = chosenTuple._1
            val trancheKey = chosenTuple._2
            val msg = {
                creator: creator,
                trancheKey: trancheKey
            }
            all {
                require(state.tranches.keys().contains(trancheKey)),
                // we are not allowing  withdrawing from fresh tranches (there was no trading)
                // @audit-info Should we handle the error at this level or should we let individual functions handle it?
                // This way is cleaner. The problem is that the simulation will not try at all then those cases where a withdraw on a non-fresh
                // tranche is attempted.
                // maybe it makes sense to use a `require` here at places where we can be confident that the code will enforce those assumptions
                require(not(state.tranches.get(trancheKey).isFresh)),

                val result = withdrawLimitOrder(state, msg)
                processResult(result, WithdrawLimitOrder(msg))
            }
        }

    action cancelLimitOrderAction(processResult: (Result, Message) => bool): bool =
        // only those addresses that have some tranches can cancel them
        val creatorKeyTuples = nontrivialTranchesOwners(state.tranchesShares, CREATORS)
        all {
            require(size(creatorKeyTuples) > 0),
            // take one of the creators that have some tranches
            nondet chosenTuple = oneOf(creatorKeyTuples)
            val creator = chosenTuple._1
            val trancheKey = chosenTuple._2
            val msg = {
                creator: creator,
                trancheKey: trancheKey
            }
            val result = cancelLimitOrder(state, msg)
            processResult(result, CancelLimitOrder(msg))
        }

    action advanceSingleBlockAction(processResult: (Result, Message) => bool): bool =
        processResult(advanceBlocks(state, 1), AdvanceTime)

    action advanceMultipleBlocksAction(processResult: (Result, Message) => bool): bool =
        nondet timestep = oneOf(1.to(MAX_FUTURE_BLOCKS))
        processResult(advanceBlocks(state, timestep), AdvanceTime)

    action advanceTimeAction(processResult: (Result, Message) => bool): bool =
        // separating the two (even though they could be combined) to
        // be able to explicitly turn off one of them
        any {
            advanceSingleBlockAction(processResult),
            advanceMultipleBlocksAction(processResult)
        }

    // INVARIANTS

    // Invariant: shares supplies are always greater or equal to zero
    val nonNegativeSharesSupply = state.pools.keys()
        .forall(poolId => state.pools.get(poolId).shares >= 0)

    // Invariant: coin and share balances are zero or greater
    val nonNegativeBalances = CREATORS.forall(creator =>
        val coins = state.coins.getOrElse(creator, Map())
        val shares = state.poolShares.getOrElse(creator, Map())
        coins.keys().forall(token => coins.get(token) >= 0) and
        shares.keys().forall(share => shares.get(share) >= 0)
        )


    // Invariant: Tranche expiration times always >= -1 (sanity check)
    val expirationTimeSanity =
        state.tranches.keys().forall(k => state.tranches.get(k).expirationTime >= -1)


    // totalMaker for a tranche equals the sum of all sharesOwned by the users
    val totalMakerInvariant = state.tranches.keys().forall(
       trancheKey =>
            val totalMaker = state.tranches.get(trancheKey).totalMaker
            val sharesOwnedSum = state.tranchesShares.keys().fold(
                0,
                (acc, creator) =>
                val sharesOwnedByUser =
                    if (state.tranchesShares.get(creator).keys().contains(trancheKey))
                        state.tranchesShares.get(creator).get(trancheKey).sharesOwned
                    else 0
                acc + sharesOwnedByUser
            )
            require(totalMaker == sharesOwnedSum)
        )

    // A user's placed orders will retain their value: what a user placed in a tranche will match the sum of
    // existing reserves (both taker and maker) and withdrawn coins (both taker and maker)
    //
    // TODO: in order to implement this invariant, I will need another bookkeeping variable to keep track of the
    // coins withdrawn from particular tranches (similar to what deposits and poolWithdraws are doing for pools)
    // Question: is this the same as noLossOnExhaustedTranches? Perhaps it is more general actually
    val makerValuePreserved = true

    // Invariant: pool reserves are always greater or equal to zero
    val nonNegativePoolReserves = state.pools.keys().forall(poolId => 
        val reserves = state.pools.get(poolId).reserves
        reserves._1 >= 0 and reserves._2 >= 0
        )

    // Sanity: All reserves are 0 or greater
    val nonNegativeTrancheAmounts = state.tranches.keys().forall(trancheKey =>
        val tranche = state.tranches.get(trancheKey)
        and{
            tranche.reservesMaker + TOLERANCE >= 0,
            tranche.reservesTaker + TOLERANCE >= 0,
            tranche.totalMaker + TOLERANCE >= 0,
            tranche.totalTaker + TOLERANCE >= 0
        }
    )

    /// If a pool has some positive amount of reserves, it also has a positive amount of shares
    val reservesImplyShares = state.pools.keys().forall(poolId => {
        val reserves = state.pools.get(poolId).reserves
        val totalShares = state.poolShares.values().fold(0, (acc, e) => acc + e.getOrElse(poolId, 0))
        (reserves._1 > 0 or reserves._2 > 0) implies totalShares > 0
    })

    /// A user who has placed a limit order will once it is fully exhausted (be
    /// it through full withdrawal or cancellation-induced withdrawal) be at no
    /// loss (with respect to the price set by tick specified by the user).
    /// We make sure that the two cases of withdrawals actually happen through witness properties
    val noLossOnExhaustedTranches = state.tranchesShares.keys().forall(addr => {
        pure def noLoss(state: State, addr: Addr, key: TrancheKey): bool = and {
            pure val placedAmount = state.bookkeeping.tranches.placements.get((addr, key))
            pure val withdrewAmount = state.bookkeeping.tranches.withdrawals.getOrElse((addr, key), 0)
            pure val canceledAmounts = state.bookkeeping.tranches.cancelations.getOrElse((addr, key), (0, 0))

            pure val totalPlaced = (placedAmount, 0)
            pure val totalWithdrew = (canceledAmounts._1, withdrewAmount + canceledAmounts._2)

            pure val placedValue = computeDepositValue(totalPlaced, key.sellTick)
            pure val withdrew = computeDepositValue(totalWithdrew, key.sellTick)

            approx_gte(withdrew, placedValue, fromInt(2))
        }

        val shareAccount = state.tranchesShares.get(addr)
        shareAccount.keys().forall(key => {
            val s = shareAccount.get(key)
            s.sharesWithdrawn == s.sharesOwned implies state.noLoss(addr, key)
        })
    })

    /* 
    The total amount of coins in the system is always preserved
     */
    val coinsPreserved = 
        val initCoins = totalCoins(initState)        
        totalCoins(state) == initCoins

    /* WITNESS PROPERTIES */
    /* These invariants should be violated. If they are not, it means the model is not reaching important states */

    /// It should be possible to exhaust a tranche through cancelation
    val exhaustedTrancheThroughCancelation = state.tranchesShares.keys().exists(addr => {
        val shareAccount = state.tranchesShares.get(addr)
        shareAccount.keys().exists(key => {
            val s = shareAccount.get(key)
            // When there is a cancelation, both of these are set to zero
            s.sharesWithdrawn == 0 and s.sharesOwned == 0
        })
    })

    /// It should be possible to exhaust a tranche through withdraws
    val exhaustedTrancheThroughWithdraw = state.tranchesShares.keys().exists(addr => {
        val shareAccount = state.tranchesShares.get(addr)
        shareAccount.keys().exists(key => {
            val s = shareAccount.get(key)
            s.sharesWithdrawn == s.sharesOwned and s.sharesWithdrawn != 0
        })
    })

    val sharedTranches = tuples(state.tranchesShares.keys(), state.tranchesShares.keys()).exists(((a, b)) =>
        a != b and state.tranchesShares.get(a).keys().intersect(state.tranchesShares.get(b).keys()) != Set()
    )

    val allInvariants = all {
        nonNegativeSharesSupply.reportIfFalse("nonNegativeSharesSupply"),
        nonNegativeBalances.reportIfFalse("nonNegativeBalances"),
        expirationTimeSanity.reportIfFalse("expirationTimeSanity"),
        totalMakerInvariant.reportIfFalse("totalMakerInvariant"),
        makerValuePreserved.reportIfFalse("makerValuePreserved"),
        nonNegativePoolReserves.reportIfFalse("nonNegativePoolReserves"),
        nonNegativeTrancheAmounts.reportIfFalse("nonNegativeTrancheAmounts"),
        reservesImplyShares.reportIfFalse("reservesImplyShares"),
        noLossOnExhaustedTranches.reportIfFalse("noLossOnExhaustedTranches"),
        coinsPreserved.reportIfFalse("coinsPreserved"),
    }
}
