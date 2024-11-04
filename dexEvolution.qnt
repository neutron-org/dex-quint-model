// -*- mode: Bluespec; -*-
module dexEvolution {
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"
    import utils.* from "utils"
    import basicSpells.* from "lib/basicSpells"
    import Dex.* from "dex"
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
        advanceTimeAction,
    }

    action assignResult(result: Result, msg: Message): bool = all {
        actionSuccess' = result.error == "nil",
        base::state' = result.state,
        postconditionOK' = if (result.error == "nil") postconditionForMessage(base::state, result.state, msg) else true,
        actionTaken' = msg
    }

    pure def postconditionForMessage(before: State, after: State, message: Message): bool = {
      match message {
        | DepositPool(msg) => depositPostcondition(before, after, msg)
        | PlaceLimitOrder(msg) => placeLimitOrderPostcondition(before, after, msg)
        | CancelLimitOrder(msg) => cancelLimitOrderPostcondition(before, after, msg)
        | WithdrawLimitOrder(msg) => withdrawLimitOrderPostcondition(before, after, msg)
        | WithdrawPool(msg) => withdrawPoolPostcondition(before, after, msg)
        | Swap(msg) => singlehopSwapPostcondition(before, after, msg)
        | _ => true
      }
    }

      action advanceTimeAction: bool =
        all {
            // separating the two (even though they could be combined) to
            // be able to explicitly turn off one of them
            any {
                base::advanceSingleBlockAction,
                base::advanceMultipleBlocksAction
            },
            postconditionOK' = postconditionOK,
            actionTaken' = AdvanceTime,
            actionSuccess' = true
        }        

    // INVARIANTS
    // Invariant: postconditions always hold
    val postconditions = postconditionOK

}
