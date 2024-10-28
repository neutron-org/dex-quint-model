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
    import runsUtils.* from "runsUtils"
    
    

    run behindTrancheEnemyLines_run =
        val creator = DEFAULT_CREATOR        
        init
        .then(
            val order: PlaceLimitOrderMsg = {
                creator: creator,
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
            val depositMsg: DepositMsg = depositMsgPool(creator, key1, (0, 10), true)
            val res = deposit(state, depositMsg)
            all{
                require(not(res.error == "nil")),
                onlyStateUpdate(res)
            }
        )
        .then(
            val key1: PoolId = {fee: 5, tick: -105, tokenPair: ("A", "B")}
            val depositMsg: DepositMsg = depositMsgPool(creator, key1, (0, 10), true)
            val res = deposit(state, depositMsg)
            all{
                // now we are behind enemy lines
                require(res.error == "nil"),
                onlyStateUpdate(res)
            }
        )

    run behindPoolEnemyLine_run = 
        val creator = DEFAULT_CREATOR
        init 
        .then(
            val key1: PoolId = {fee: 5, tick: -100, tokenPair: ("A", "B")}
            val depositMsg: DepositMsg = depositMsgPool(creator, key1, (10, 10), true)
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
            val depositMsg: DepositMsg = depositMsgPool(creator, key1, (10, 10), true)
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
            val depositMsg: DepositMsg = depositMsgPool(creator, key1, (10, 10), true)
            val res = deposit(state, depositMsg)
            all {
                state' = res.state,
                require(res.error == "Pool behind enemy lines"),
                actionSuccess' = false,
                actionTaken' = {name: "DepositPool", message: DepositPoolSubType(depositMsg)},
                postconditionOK' = true
            }
        )
    
    

    run limitOrderTrade_run = 
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