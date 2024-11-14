// -*- mode: Bluespec; -*-
module dexEvolution {
    import basicSpells.* from "lib/basicSpells"
    import dec from "lib/dec"
    import types.* from "types"
    import utils.* from "utils"
    import postconditions.* from "postconditions"
    import bookkeeping.* from "bookkeeping"
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
        actionSuccess' = true,
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

    // INVARIANTS

    // Invariant: postconditions always hold
    val postconditions = postconditionOK

    val allInvariants = all {
        base::allInvariants,
        postconditionOK.reportIfFalse("postconditionOK"),
    }
}
