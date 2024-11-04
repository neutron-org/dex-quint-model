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

    pure def sharesGained(before: State, after: State, creator: Addr, poolKey: PoolId): int = {
        val beforeShares = before.poolShares.get(creator).getOrElse(poolKey, 0)
        val afterShares = after.poolShares.get(creator).getOrElse(poolKey, 0)
        afterShares - beforeShares
    }

    pure def coinsGained(before: State, after: State, creator: Addr, token: Token): int = {
        val beforeBalance = before.coins.get(creator).get(token)
        val afterBalance = after.coins.get(creator).get(token)
        afterBalance - beforeBalance
    }

    pure def withdrawMsgPool(creator: Addr, poolKey: PoolId, sharesAmount: int): WithdrawPoolMsg = 
        {
            creator: creator,
            tokenPair: poolKey.tokenPair,
            sharesAmount: sharesAmount,
            tick: poolKey.tick,
            fee: poolKey.fee
        }

    }