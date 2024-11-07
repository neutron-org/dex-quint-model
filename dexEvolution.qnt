// -*- mode: Bluespec; -*-
module dexEvolution {
    import basicSpells.* from "lib/basicSpells"
    import dec from "lib/dec"
    import types.* from "types"
    import utils.* from "utils"
    import postconditions.* from "postconditions"
    import baseDexEvolution as base from "baseDexEvolution"

    // EXTRA STATE VARIABLES
    var postconditionOK: bool
    var actionSuccess: bool
    var actionTaken: Message

    // STATE MACHINE
    action init = all {
        base::init,
        postconditionOK' = true,
        actionTaken' = Init,
        actionSuccess' = true
    }

    action step: bool = any {
        base::placeLimitOrderAction(assignResult),
        base::cancelLimitOrderAction(assignResult),
        base::withdrawLimitOrderAction(assignResult),
        base::withdrawPoolAction(assignResult),
        base::depositAction(assignResult),
        base::singlehopSwapAction(assignResult),
        base::advanceTimeAction(assignResult),
    }

    action assignResult(result: Result, msg: Message): bool = all {
        actionSuccess' = result.error == "nil",
        base::state' = result.state,
        postconditionOK' = if (result.error == "nil") postconditionForMessage(base::state, result.state, msg) else true,
        actionTaken' = msg
    }

    pure def postconditionForMessage(before: State, after: State, message: Message): bool = {
      match message {
        | DepositPool(msg) => depositPostcondition(before, after, msg) and nonDecreasingPools(before, after, msg.tokenPair)
        | PlaceLimitOrder(msg) => placeLimitOrderPostcondition(before, after, msg) and nonDecreasingPools(before, after, (msg.tokenIn, msg.tokenOut))
        | CancelLimitOrder(msg) => cancelLimitOrderPostcondition(before, after, msg)
        | WithdrawLimitOrder(msg) => withdrawLimitOrderPostcondition(before, after, msg)
        | WithdrawPool(msg) => withdrawPoolPostcondition(before, after, msg)
        | Swap(msg) => singlehopSwapPostcondition(before, after, msg) and nonDecreasingPools(before, after, msg.tokenPair)
        | _ => true
      }
    }

    // INVARIANTS

    // Invariant: postconditions always hold
    val postconditions = postconditionOK

    /// After each singlehopSwap, deposit, or placeLimitOrder, the values of
    /// pools with the relevant pair has remained the same or it has increased.
    pure def nonDecreasingPools(before: State, after: State, tokenPair: TokenPair): bool = {
        pure def relevantPoolIdsBefore = before.pools.keys().filter(p => p.tokenPair == tokenPair)
        pure def relevantPoolIdsAfter = after.pools.keys().filter(p => p.tokenPair == tokenPair)

        and {
            relevantPoolIdsBefore.subseteq(relevantPoolIdsAfter),
            relevantPoolIdsBefore.forall(p => {
                dec::approx_gte(
                    computePoolValue(after.pools.get(p).reserves, p.tick),
                    computePoolValue(before.pools.get(p).reserves, p.tick),
                    dec::one
                )
            })
        }
    }

    /// A user who has placed a limit order will once it is fully exhausted (be
    /// it through full withdrawal or cancellation-induced withdrawal) be at no
    /// loss (with respect to the price set by tick specified by the user).
    /// TODO: We shall not model it as an implication only, but instead we
    /// should enforce that there were indeed some full withdrawals
    val noLossOnExhaustedTranches = base::state.tranchesShares.keys().forall(addr => {
        pure def noLoss(state: State, addr: Addr, key: TrancheKey): bool = and {
            pure val depositedAmount = state.bookkeeping.tranches.deposits.get((addr, key))
            pure val withdrewAmount = state.bookkeeping.tranches.withdrawals.getOrElse((addr, key), 0)
            pure val canceledAmounts = state.bookkeeping.tranches.cancelations.getOrElse((addr, key), (0, 0))

            pure val totalDeposited = (depositedAmount, 0)
            pure val totalWithdrew = (canceledAmounts._1, withdrewAmount + canceledAmounts._2)

            pure val deposited = computeDepositValue(totalDeposited, key.sellTick)
            pure val withdrew = computeDepositValue(totalWithdrew, key.sellTick)
            dec::approx_gte(withdrew, deposited, dec::fromInt(2))
        }

        val shareAccount = base::state.tranchesShares.get(addr)
        shareAccount.keys().forall(key => {
            val s = shareAccount.get(key)
            s.sharesWithdrawn == s.sharesOwned implies base::state.noLoss(addr, key)
        })
    })
}
