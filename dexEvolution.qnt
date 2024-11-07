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
}
