// -*- mode: Bluespec; -*-
module violationRuns {
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"
    import utils.* from "utils"
    import basicSpells.* from "lib/basicSpells"
    import Dex.* from "dex"
    import dexEvolution.* from "dexEvolution"
    import postconditions.* from "postconditions"
    import runsUtils.* from "runsUtils"
    
        
// ---- The run illustrating the violation of [SHARES-FOR-DEPOSITS] property
    val testPoolKey: PoolId = {fee: 4, tick: -2, tokenPair: DEFAULT_TOKEN_PAIR}          
    action single_small_deposit = 
        val msg: DepositMsg = {
                amountPair: (0, 1),
                autoswap: false,
                creator: DEFAULT_CREATOR,
                fee: testPoolKey.fee,
                tick: testPoolKey.tick,
                tokenPair: testPoolKey.tokenPair
            }
            val res = deposit(state, msg)
            onlyStateUpdate(res)
    
    run withdrawIssue = 
        init
        .then(
            all{
                single_small_deposit,                
            }            
        )
        .then(
            all{
                onlyStateUpdate(ok(state)),                
                require(                    
                    // The weird formulation is to support both cases,
                    // with FORBID_ZERO_SHARES = false and FORBID_ZERO_SHARES = true
                    state.deposits.get(DEFAULT_CREATOR).getOrElse(testPoolKey, EMPTY_DEPOSIT_DATA)
                        .amounts._2 > 0
                    implies
                    state.pools.get(testPoolKey).shares > 0
                )
            }
        )
    
    // illustrates the violation of the [RESERVES-IMPLY-SHARES] property
    run repeatedWithdrawIssue = 
        val numRepetitions = 1000
        init
        .then(
            all{
                numRepetitions.reps(_ => single_small_deposit),                
            }            
        )
        .then(
            all{
                onlyStateUpdate(ok(state)),
                require(                    
                    // The weird formulation is to support both cases,
                    // with FORBID_ZERO_SHARES = false and FORBID_ZERO_SHARES = true
                    state.deposits.get(DEFAULT_CREATOR).getOrElse(testPoolKey, EMPTY_DEPOSIT_DATA)
                    // in each repetition we deposit 1, so now we have an arbitrary amount of token1
                        .amounts._2 > numRepetitions-1
                    implies
                    // but still no shares at all
                    state.pools.get(testPoolKey).shares > 0
                )
            }
        )
    
    

    
    
    // The runs showcases how a reserves ratio changed from
    // 1/73 = 0.0136 to 4/724 = 0.0055
    // In the inverse, this is going from 73 to 181
    run autoswapIssue = 
        val creator = DEFAULT_CREATOR
        init 
        .then(
            val depositMsg: DepositMsg = depositMsgPool(creator, testPoolKey, (1, 73), false)
            val res = deposit(state, depositMsg)
            val updatedPool = res.state.pools.get(testPoolKey)
            all{
                onlyStateUpdate(res),
                assert(updatedPool.reservesRatio() == (1, 73))
            }
        )
        .then(
            val depositMsg: DepositMsg = depositMsgPool(creator, testPoolKey, (21, 145), false)
            val res = deposit(state, depositMsg)
            val updatedPool = res.state.pools.get(testPoolKey)
            all{
                onlyStateUpdate(res),
                assert(updatedPool.reservesRatio() == (2, 218))
            })
        .then(
            val depositMsg: DepositMsg = depositMsgPool(creator, testPoolKey, (21, 217), false)
            val res = deposit(state, depositMsg)
            val updatedPool = res.state.pools.get(testPoolKey)
            all{
                onlyStateUpdate(res),
                assert(updatedPool.reservesRatio() == (3, 435))
            })
        .then(
            val depositMsg: DepositMsg = depositMsgPool(creator, testPoolKey, (21, 289), false)
            val res = deposit(state, depositMsg)
            val updatedPool = res.state.pools.get(testPoolKey)
            all{
                onlyStateUpdate(res),
                assert(updatedPool.reservesRatio() == (4, 724))
            })

       
    }