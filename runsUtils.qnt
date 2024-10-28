// -*- mode: Bluespec; -*-
module runsUtils {
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"
    import utils.* from "utils"
    import basicSpells.* from "lib/basicSpells"
    import Dex.* from "dex"
    import dexEvolution.* from "dexEvolution"
    import postconditions.* from "postconditions"

    action unchangedState = all {
        state' = state,
        postconditionOK' = postconditionOK,
        actionTaken' = actionTaken,
        actionSuccess' = actionSuccess

    }

    pure def depositMsgPool(creator: Addr, poolKey: PoolId, amountPair: (int, int), doAutoswap: bool): DepositMsg = {
        creator: creator,
        tokenPair: poolKey.tokenPair,
        amountPair: amountPair,
        tick: poolKey.tick,
        fee: poolKey.fee,
        autoswap: doAutoswap
    }

    // Makes the update to the state, and all other updates are "dummy" updates
    action onlyStateUpdate(res: Result): bool =         
        val updatedState = res.state                        
        all{
            state' = updatedState,
            postconditionOK' = false,
            actionSuccess' = res.error == "nil",
            actionTaken' = {name: "placeholder", message: Init},
        }
    }