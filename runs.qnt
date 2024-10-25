// -*- mode: Bluespec; -*-
module runs {
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

    // Makes the update to the state, and all other updates are "dummy" updates
    action onlyStateUpdate(res: Result): bool =         
        val updatedState = res.state                        
        all{
            state' = updatedState,
            postconditionOK' = false,
            actionSuccess' = res.error == "nil",
            actionTaken' = {name: "placeholder", message: Init},
        }
        

    val testPoolKey: PoolId = {fee: -2, tick: 2, tokenPair: ("A", "B")}
    val testCreator = "Alice"
    pure def depositMsgPool(poolKey: PoolId, amountPair: (int, int), doAutoswap: bool): DepositMsg = {
        creator: testCreator,
        tokenPair: poolKey.tokenPair,
        amountPair: amountPair,
        tick: poolKey.tick,
        fee: poolKey.fee,
        autoswap: doAutoswap
    }
    action helper = 
        val msg: DepositMsg = {
                amountPair: (0, 1),
                autoswap: false,
                creator: "Carol",
                fee: 4,
                tick: -2,
                tokenPair: ("A", "C")
            }
            val res = deposit(state, msg)
            onlyStateUpdate(res)
    
    run withdrawIssue = 
        init
        .then(
            all{
                helper,                
            }            
        )
        .then(
            all{
                onlyStateUpdate(ok(state)),
                val key: PoolId = {fee: 4, tick: -2, tokenPair: ("A", "C")}
                require(                    
                    // The weird formulation is to support both cases,
                    // with FORBID_ZERO_SHARES = false and FORBID_ZERO_SHARES = true
                    size(state.pools.keys()) > 0
                    implies
                    state.pools.get(key).shares > 0
                )
            }
        )
    
    run repeatedWithdrawIssue = 
        init
        .then(
            all{
                1000.reps(_ => helper),                
            }            
        )
        .then(
            all{
                onlyStateUpdate(ok(state)),
                val key: PoolId = {fee: 4, tick: -2, tokenPair: ("A", "C")}
                require(                    
                    // The weird formulation is to support both cases,
                    // with FORBID_ZERO_SHARES = false and FORBID_ZERO_SHARES = true
                    size(state.pools.keys()) > 0
                    implies
                    state.pools.get(key).shares > 0
                )
            }
        )
    
    

    run behindTrancheEnemyLines = 
        init
        .then(
            val order: PlaceLimitOrderMsg = {
                creator: "Alice",
                orderType: GoodTillCancelled,
                tokenIn: "A",
                tokenOut: "B",
                amountIn: 10,
                tick: 100,
                maxAmountOut: -1
            }
            val res = placeLimitOrder(state, order)
            onlyStateUpdate(res)
        )
        .then(
            val key1: PoolId = {fee: 5, tick: -106, tokenPair: ("A", "B")}
            val depositMsg: DepositMsg = depositMsgPool(key1, (0, 10), true)
            val res = deposit(state, depositMsg)
            all{
                require(not(res.error == "nil")),
                onlyStateUpdate(res)
            }
        )
        .then(
            val key1: PoolId = {fee: 5, tick: -105, tokenPair: ("A", "B")}
            val depositMsg: DepositMsg = depositMsgPool(key1, (0, 10), true)
            val res = deposit(state, depositMsg)
            all{
                require(res.error == "nil"),
                onlyStateUpdate(res)
            }
        )

    run behindPoolEnemyLine = 
        init 
        .then(
            val key1: PoolId = {fee: 5, tick: -100, tokenPair: ("A", "B")}
            val depositMsg: DepositMsg = depositMsgPool(key1, (10, 10), true)
            val res = deposit(state, depositMsg)
            all{
                require(res.error == "nil"),
                state' = res.state,
                actionSuccess' = true,
                postconditionOK' = true,
                actionTaken' = {name: "DepositPool", message: DepositPoolSubType(depositMsg)}
            }            
        ) 
        .then(
            val key1: PoolId = {fee: 3, tick: -100, tokenPair: ("A", "B")}
            val depositMsg: DepositMsg = depositMsgPool(key1, (10, 10), true)
            val res = deposit(state, depositMsg)
            all{
                require(res.error == "nil"),
                state' = res.state,
                actionSuccess' = true,
                postconditionOK' = true,
                actionTaken' = {name: "DepositPool", message: DepositPoolSubType(depositMsg)}
            }            
        )        
        .then(
            val key1: PoolId = {fee: 5, tick: -80, tokenPair: ("A", "B")}
            val depositMsg: DepositMsg = depositMsgPool(key1, (10, 10), true)
            val res = deposit(state, depositMsg)
            all {
                state' = res.state,
                require(res.error == "Pool behind enemy lines"),
                actionSuccess' = false,
                actionTaken' = {name: "DepositPool", message: DepositPoolSubType(depositMsg)},
                postconditionOK' = true
            }
        )
    
    // The runs showcases how a reserves ratio changed from
    // 1/73 = 0.0136 to 4/724 = 0.0055
    // In the inverse, this is going from 73 to 181
    run autoswapIssue = 
        init 
        .then(
            val depositMsg: DepositMsg = depositMsgPool(testPoolKey, (1, 73), false)
            val res = deposit(state, depositMsg)
            val updatedPool = res.state.pools.get(testPoolKey)
            all{
                onlyStateUpdate(res),
                assert(updatedPool.reservesRatio() == (1, 73))
            }
        )
        .then(
            val depositMsg: DepositMsg = depositMsgPool(testPoolKey, (21, 145), false)
            val res = deposit(state, depositMsg)
            val updatedPool = res.state.pools.get(testPoolKey)
            all{
                onlyStateUpdate(res),
                assert(updatedPool.reservesRatio() == (2, 218))
            })
        .then(
            val depositMsg: DepositMsg = depositMsgPool(testPoolKey, (21, 217), false)
            val res = deposit(state, depositMsg)
            val updatedPool = res.state.pools.get(testPoolKey)
            all{
                onlyStateUpdate(res),
                assert(updatedPool.reservesRatio() == (3, 435))
            })
        .then(
            val depositMsg: DepositMsg = depositMsgPool(testPoolKey, (21, 289), false)
            val res = deposit(state, depositMsg)
            val updatedPool = res.state.pools.get(testPoolKey)
            all{
                onlyStateUpdate(res),
                assert(updatedPool.reservesRatio() == (4, 724))
            })

    run limitOrderTrade = 
        init
        .then(
            val bobsOrder: PlaceLimitOrderMsg = {
                creator: "Bob",
                orderType: GoodTillCancelled,
                tokenIn: "A",
                tokenOut: "B",
                amountIn: 10,
                tick: 19460,
                maxAmountOut: -1
            }
            val updatedState = placeLimitOrder(state, bobsOrder).state
            all{
                state' = updatedState,
                postconditionOK' = placeLimitOrderPostcondition(state, bobsOrder, updatedState),
                actionTaken' = {name: "PlaceLimitOrder", message: PlaceLimitOrderSubType(bobsOrder)},
                actionSuccess' = true,
                val trancheKey = { id: 1, makerToken: "A", takerToken: "B", tick: 19460 }
                val updatedTranche = updatedState.tranches.get(trancheKey)
                all{
                    assert(updatedTranche.reservesMaker == 10),
                    assert(updatedTranche.reservesTaker == 0),
                    assert(updatedTranche.tick == 19460),
                    assert(updatedTranche.makerDenomination == "A")
                }
            }            
        )        
        // Run Alice's order to match the Bob's existing order
        .then(
            val alicesOrder: PlaceLimitOrderMsg = {
                creator: "Alice",
                orderType: GoodTillCancelled,
                tokenIn: "B",
                tokenOut: "A",
                amountIn: 20,
                tick: 20000,
                maxAmountOut: -1
            }
            val updatedState = placeLimitOrder(state, alicesOrder).state
            
            all{
                state' = updatedState,
                actionSuccess' = true,
                postconditionOK' = placeLimitOrderPostcondition(state, alicesOrder, updatedState),
                actionTaken' = {name: "PlaceLimitOrder", message: PlaceLimitOrderSubType(alicesOrder)},
                val trancheKey = { id: 1, makerToken: "A", takerToken: "B", tick: 19460 }
                val updatedTranche = updatedState.tranches.get(trancheKey)
                all{
                    assert(updatedTranche.reservesMaker == 7),
                    assert(updatedTranche.reservesTaker == 20),
                }
                
            }        
        )
        // Run again Alice's order to completely fill Bob's order and create a new tranche
        .then(
            val alicesOrder: PlaceLimitOrderMsg = {
                creator: "Alice",
                orderType: GoodTillCancelled,
                tokenIn: "B",
                tokenOut: "A",
                amountIn: 20000,
                tick: 20000,
                maxAmountOut: -1
            }
            val updatedState = placeLimitOrder(state, alicesOrder).state            
            all{
                state' = updatedState,
                actionSuccess' = true,
                postconditionOK' = placeLimitOrderPostcondition(state, alicesOrder, updatedState),
                actionTaken' = {name: "PlaceLimitOrder", message: PlaceLimitOrderSubType(alicesOrder)},
                val newTrancheKey = { id: 2, makerToken: "B", takerToken: "A", tick: 20000 }
                val newTranche = updatedState.tranches.get(newTrancheKey)
                all{
                    assert(newTranche.reservesMaker == 19952),
                    assert(newTranche.reservesTaker == 0),
                    assert(newTranche.makerDenomination == "B")
                }                
            }
        )    
    }