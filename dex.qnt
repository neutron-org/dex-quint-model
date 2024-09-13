// -*- mode: Bluespec; -*-

/**
 * The core of the Neutron DEX.
 * 
 *
 * 
 *
 *
 */
module Dex {
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"
    import utils.* from "utils"
    
    var state: State    

    pure def placeLimitOrder(state: State, msg: PlaceLimitOrderMsg) : State =
    
        val normalizedCoinAndTick = normalizeCoinAndTick(msg.tokenPair, msg.tick)        
        val tick1to0 = normalizedCoinAndTick._3
        val price1To0 = priceFromTick(tick1to0)
        // TODO: unfinished
        state


    pure def cancelLimitOrder(state: State, msg: CancelLimitOrderMsg) : State =
        // TODO: unfinished
        state 


    pure def withdrawLimitOrder(state: State, msg: WithdrawLimitOrderMsg) : State =
        // TODO: unfinished
        state   

    action placeLimitOrderAction: bool = 
        nondet c = oneOf(CREATORS)
        nondet o = oneOf(ORDER_TYPES)
        nondet t = oneOf(TICKS)
        nondet token_pair = oneOf(TOKEN_PAIRS)        
        val limitOrderMsg = {
            creator: c,
            orderType: o,
            tick: t,
            tokenPair: token_pair
        }
        state' = placeLimitOrder(state, limitOrderMsg)


    action cancelLimitOrderAction: bool =
        nondet creator = oneOf(state.shares.keys())
        nondet trancheKey = oneOf(state.shares.get(creator).keys())
        val cancelLimitOrderMsg = {
            creator: creator,
            trancheKey: trancheKey
        }
        state' = cancelLimitOrder(state, cancelLimitOrderMsg)

    
    action withdrawLimitOrderAction: bool = 
        nondet creator = oneOf(state.shares.keys())
        nondet trancheKey = oneOf(state.shares.get(creator).keys())
        val withdrawLimitOrderMsg = {
            creator: creator,
            trancheKey: trancheKey
        }
        all {
            require(state.tranches.keys().contains(trancheKey)),
            require(not(state.tranches.get(trancheKey).isFresh)),
            state' = withdrawLimitOrder(state, withdrawLimitOrderMsg)
        }
    
    action advanceTimeAction: bool = 
        nondet t = oneOf(1.to(MAX_FUTURE_BLOCKS))
        state' = {
            blockNumber: state.blockNumber + t,
            ... state
        }
        

    action step: bool = any {
        placeLimitOrderAction,

        cancelLimitOrderAction,

        withdrawLimitOrderAction,

        advanceTimeAction

    }



    }