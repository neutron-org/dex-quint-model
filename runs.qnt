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
            actionTaken' = {name: "DeposixxtPool", message: Init},
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