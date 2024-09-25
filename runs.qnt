module runs {
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"
    import utils.* from "utils"
    import basicSpells.* from "basicSpells"
    import Dex.* from "dex"

    action unchangedState = all {
        state' = state,
        postconditionOK' = postconditionOK,
        actionTaken' = actionTaken

    }

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
            val updatedState = placeLimitOrder(state, bobsOrder)
            all{
                state' = updatedState,
                postconditionOK' = placeLimitOrderPostcondition(state, bobsOrder, updatedState),
                actionTaken' = {name: "PlaceLimitOrder", message: PlaceLimitOrderSubType(bobsOrder)},
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
            val updatedState = placeLimitOrder(state, alicesOrder)
            
            all{
                state' = updatedState,
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
            val updatedState = placeLimitOrder(state, alicesOrder)            
            all{
                state' = updatedState,
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