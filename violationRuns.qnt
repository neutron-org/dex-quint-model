// -*- mode: Bluespec; -*-
module violationRuns {
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"
    import utils.* from "utils"
    import basicSpells.* from "lib/basicSpells"
    import Dex.* from "dex"
    import baseDexEvolution.* from "baseDexEvolution"
    import postconditions.* from "postconditions"
    import runsUtils.* from "runsUtils"
    
    // ------ The run illustrating the violation of the [NO-LOSS] and [NO-GAIN] properties
    // It is a cleared version of the generated trace violations/noLossViolation.itf.json
    run noLossViolation = 
        val key = {fee: 6, tick: -9, tokenPair: ("B", "C")}
        init.then(
            val msg = depositMsgPool("Bob", key, (211,337), true)
            val res = deposit(state, msg)
            all {
                state' = res.state,
                require(res.error == "nil"),                
                require(sharesGained(state, res.state, "Bob", key) == 547)
            })
        .then(
            val msg = withdrawMsgPool("Bob", key, 82)
            val res = withdrawPool(state, msg)
            all {
                state' = res.state,
                require(res.error == "nil"),
                require(coinsGained(state, res.state, "Bob", "B") == 31),
                require(coinsGained(state, res.state, "Bob", "C") == 50)
            })
        .then(
            val msg = depositMsgPool("Carol", key, (927, 11), false)
            val res = deposit(state, msg)
            all {
                state' = res.state,
                require(res.error == "nil"),
                // only gets 16 due to autoswap=false
                require(sharesGained(state, res.state, "Carol", key) == 16)
            })
        .then(
            val msg = withdrawMsgPool("Carol", key, 7)
            val res = withdrawPool(state, msg)
            all {
                state' = res.state,
                require(res.error == "nil"),
                require(coinsGained(state, res.state, "Carol", "B") == 2),
                require(coinsGained(state, res.state, "Carol", "C") == 4)
            })
        .then(
            val msg = withdrawMsgPool("Carol", key, 4)
            val res = withdrawPool(state, msg)
            all {
                state' = res.state,
                require(res.error == "nil"),
                require(coinsGained(state, res.state, "Carol", "B") == 1),
                require(coinsGained(state, res.state, "Carol", "C") == 2)
            })
        .then(
            val msg = withdrawMsgPool("Carol", key, 1)
            val res = withdrawPool(state, msg)
            all {
                state' = res.state,
                require(res.error == "nil"),
                require(coinsGained(state, res.state, "Carol", "B") == 0),
                require(coinsGained(state, res.state, "Carol", "C") == 0)
            })
        .then(
            val msg = withdrawMsgPool("Bob", key, 308)
            val res = withdrawPool(state, msg)
            all {
                state' = res.state,
                require(res.error == "nil"),
                require(coinsGained(state, res.state, "Bob", "B") == 120),
                require(coinsGained(state, res.state, "Bob", "C") == 191)
            })
        .then(
            val msg = withdrawMsgPool("Carol", key, 3)
            val res = withdrawPool(state, msg)
            all {
                state' = res.state,
                require(res.error == "nil"),
                require(coinsGained(state, res.state, "Carol", "B") == 1),
                require(coinsGained(state, res.state, "Carol", "C") == 1)
            })
        .then(
            val msg = withdrawMsgPool("Carol", key, 1)
            val res = withdrawPool(state, msg)
            all {
                state' = res.state,
                require(res.error == "nil"),
                require(coinsGained(state, res.state, "Carol", "B") == 0),
                require(coinsGained(state, res.state, "Carol", "C") == 0)
            })
        .expect(
            val carolDeposited = computeDepositValue(state.bookkeeping.pools.deposits.get(("Carol", key)), key.tick)
            val carolWithdrew = computeDepositValue(state.bookkeeping.pools.withdrawals.get(("Carol", key)), key.tick)
            all {
                state.poolShares.get("Carol").get(key) == 0,
                carolDeposited.sub(carolWithdrew).truncate() == 5
            })
        .then(
            val msg = withdrawMsgPool("Bob", key, 157)
            val res = withdrawPool(state, msg)
            all {
                state' = res.state,
                require(res.error == "nil"),
                require(coinsGained(state, res.state, "Bob", "B") == 62),
                require(coinsGained(state, res.state, "Bob", "C") == 100)
            })
        .expect(all {
            require(state.poolShares.get("Bob").get(key) == 0),
            val bobDeposited = computeDepositValue(state.bookkeeping.pools.deposits.get(("Bob", key)), key.tick)
            val bobWithdrew = computeDepositValue(state.bookkeeping.pools.withdrawals.get(("Bob", key)), key.tick)
            // Bob has gained something (even though there were no swaps, so he gained what Carol has lost)
            bobWithdrew.sub(bobDeposited).truncate() == 5
        })
    
    // ------ The run illustrating the violation of the [POOL-RATIO-CONSTANT] property
    // The runs showcases how a reserves ratio changed from
    // 1/73 = 0.0136 to 4/724 = 0.0055
    // In the inverse, this is going from 73 to 181
    val testPoolKey: PoolId = {fee: 4, tick: -2, tokenPair: DEFAULT_TOKEN_PAIR}
    run autoswapIssue = 
        val creator = DEFAULT_CREATOR
        init 
        .then(
            val depositMsg: DepositMsg = depositMsgPool(creator, testPoolKey, (1, 73), false)
            val res = deposit(state, depositMsg)
            val updatedPool = res.state.pools.get(testPoolKey)
            all {
                state' = res.state,
                assert(updatedPool.reservesRatio() == (1, 73))
            }
        )
        .then(
            val depositMsg: DepositMsg = depositMsgPool(creator, testPoolKey, (21, 145), false)
            val res = deposit(state, depositMsg)
            val updatedPool = res.state.pools.get(testPoolKey)
            all {
                state' = res.state,
                assert(updatedPool.reservesRatio() == (2, 218))
            })
        .then(
            val depositMsg: DepositMsg = depositMsgPool(creator, testPoolKey, (21, 217), false)
            val res = deposit(state, depositMsg)
            val updatedPool = res.state.pools.get(testPoolKey)
            all {
                state' = res.state,
                assert(updatedPool.reservesRatio() == (3, 435))
            })
        .then(
            val depositMsg: DepositMsg = depositMsgPool(creator, testPoolKey, (21, 289), false)
            val res = deposit(state, depositMsg)
            val updatedPool = res.state.pools.get(testPoolKey)
            all {
                state' = res.state,
                assert(updatedPool.reservesRatio() == (4, 724))
            })
    }
