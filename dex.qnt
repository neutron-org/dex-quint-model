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
    var postViolation: bool

    // this function returns:
    //  - a mapping from poolIds to amounts that should be swapped with this pool
    //  - a mapping from trancheKeys to amounts that should be filled with this tranche
    //  - an integer saying how much of the order amount is still remaining
    pure def computeMakerOrderMatches(state: State, msg: PlaceLimitOrderMsg) : (PoolId->int, TrancheKey-> int, int) = 
        (Map(), Map(), -1)

    pure def swap(state: State, poolId: PoolId, amount: int) : State = state

    pure def fillTranche(state: State, trancheKey: TrancheKey, amount: int) : State = state

    pure def createNewTranche(state: State, msg: PlaceLimitOrderMsg, amount: int) : State = state

    pure def placeLimitOrder(state: State, msg: PlaceLimitOrderMsg) : State =
        // TODO: this should be separated into the case when the swap happens, and when the maker order is placed
        // Depending on the kind of order, a part should be immediately swapped and a part may still be placed as a maker order        
        
        val fulfilledOrderInfo = computeMakerOrderMatches(state, msg)
        val poolsToSwap = fulfilledOrderInfo._1
        val tranchesToFill = fulfilledOrderInfo._2
        val remainingAmount = fulfilledOrderInfo._3
        val stateAfterSwaps = poolsToSwap.keys().fold(
            state,
            (state, poolId) => swap(state, poolId, poolsToSwap.get(poolId))
        )

        val stateAfterFills = tranchesToFill.keys().fold(
            stateAfterSwaps,
            (state, trancheKey) => fillTranche(state, trancheKey, tranchesToFill.get(trancheKey))
        )

        // if there is still some amount left, we need to create a new tranche
        createNewTranche(stateAfterFills, msg, remainingAmount)

    pure def computeWithdrawShares(percentFilled: Dec, userNonWithdrawnTrancheShare: Dec): int =
        // @audit-issue Should this be truncate? Rounding discussion
        truncate(
            mul(
                percentFilled,
                userNonWithdrawnTrancheShare
            )
        )

    pure def computeCancelAmountMakerToken(percentUnfilled: Dec, userTotalTrancheShare: Dec, totalMaker: int): int =
        // @audit-issue Already here we are running into rounding problems. Would be great to get the description on rounding
        // that we discussed in one of the previous meetings
        truncate(
            mulInt(
                mul(
                    percentUnfilled,
                    userTotalTrancheShare
                ),
                totalMaker
            )
        )
    
    pure def computeWithdrawAmountTakerToken(percentFilled: Dec, userNonWithdrawnTrancheShare: Dec, totalTaker: int): int = 
        truncate(
            mulInt(
                mul(
                    percentFilled,
                    userNonWithdrawnTrancheShare
                ),
                totalTaker
            )
                )
    

    pure def cancelLimitOrderPostcondition(before: State, msg: CancelLimitOrderMsg, after: State) : bool =        
        val tranche_before: Tranche = before.tranches.get(msg.trancheKey)
        val users_tranches_before = before.tranches_shares.get(msg.creator).keys()
        val users_tranches_after = after.tranches_shares.get(msg.creator).keys()
        and {
            // there is at most one removed tranche from the set of users' tranches
            // and this is exactly the tranche that the user requested to be cancelled
            val removedTranches = users_tranches_before.exclude(users_tranches_after)
            and{
                size(removedTranches) <= 1,
                removedTranches.forall(
                    trancheKey => trancheKey == msg.trancheKey
                    )
            },
            
            // user received their fair amount of maker tokens back            
            // this calculation corresponds to the point 2a from the spec document
            // I am wondering if it is the same as `percentUnfilled * totalAmountOfUserShares` ==> pending confirmation from the dev team
            val percentUnfilled = calculatePercentUnfilled(before.tranches.get(msg.trancheKey))
            val userTotalTrancheShare = calculateUserTotalTrancheShare(tranche_before, before.tranches_shares.get(msg.creator))                    
            // @audit-info I am using "MakerToken" instead of "TokenIn". Is there a good reason to stick to "TokenIn" and "TokenOut" language?
            val cancelAmountOutMakerToken =                 
                computeCancelAmountMakerToken(percentUnfilled, userTotalTrancheShare, tranche_before.totalMaker)
            and{
                // the user's balance has increased for what was cancelled
                cancelAmountOutMakerToken == 
                after.coins.get(msg.creator).get(tranche_before.makerDenomination) -
                before.coins.get(msg.creator).get(tranche_before.makerDenomination),
                // the totalMaker has decreased for what was cancelled
                cancelAmountOutMakerToken ==
                before.tranches.get(msg.trancheKey).totalMaker -
                after.tranches.get(msg.trancheKey).totalMaker

            },
            
            // This part corresponds to the point 2b from the spec document
            val percentFilled = calculatePercentFilled(before.tranches.get(msg.trancheKey))     
            val userNonWithdrawnTrancheShare = calculateUserNonWithdrawnTrancheShare(tranche_before, before.tranches_shares.get(msg.creator))       
            val cancelAmountOutTakerToken = 
                // we are using "computeWITHDRAW..." because when cancelling, the user is also withdrawing their unwithdrawn share
                computeWithdrawAmountTakerToken(percentFilled, userNonWithdrawnTrancheShare, tranche_before.totalTaker)

            and{
                // the user's taker token balance has increased for what was withdrawn when cancelling
                cancelAmountOutTakerToken == 
                    after.coins.get(msg.creator).get(tranche_before.takerDenomination) -
                    before.coins.get(msg.creator).get(tranche_before.takerDenomination),
                // the totalTaker has decreased for what was cancelled
                cancelAmountOutTakerToken ==
                    before.tranches.get(msg.trancheKey).totalTaker -
                    after.tranches.get(msg.trancheKey).totalTaker
            },
            
            // make sure that the user's withdrawn shares are updated correctly                        
            before.tranches_shares.get(msg.creator).get(msg.trancheKey).sharesOwned 
            ==
            after.tranches_shares.get(msg.creator).get(msg.trancheKey).sharesWithdrawn                           
        }



    pure def cancelLimitOrder(state: State, msg: CancelLimitOrderMsg) : State =
        // TODO: unfinished
        state 

    pure def withdrawLimitOrderPostcondition(before: State, msg: WithdrawLimitOrderMsg, after: State) : bool = 
        val withdrawnShares: int = computeWithdrawShares(
            calculatePercentFilled(before.tranches.get(msg.trancheKey)),
            calculateUserNonWithdrawnTrancheShare(before.tranches.get(msg.trancheKey), before.tranches_shares.get(msg.creator))
        )
        val withdrawnAmountTakerToken: int = computeWithdrawAmountTakerToken(
            calculatePercentFilled(before.tranches.get(msg.trancheKey)),
            calculateUserNonWithdrawnTrancheShare(before.tranches.get(msg.trancheKey), before.tranches_shares.get(msg.creator)),
            before.tranches.get(msg.trancheKey).totalTaker
        )
        and {
            // the user's taker balance has increased for what was withdrawn
            withdrawnAmountTakerToken == 
                after.coins.get(msg.creator).get(before.tranches.get(msg.trancheKey).takerDenomination) -
                before.coins.get(msg.creator).get(before.tranches.get(msg.trancheKey).takerDenomination),
            // the totalTaker has decreased for what was withdrawn
            withdrawnAmountTakerToken ==
                before.tranches.get(msg.trancheKey).totalTaker -
                after.tranches.get(msg.trancheKey).totalTaker,
            // the user's shares withdrawn have increased appropriately
            withdrawnShares == 
                after.tranches_shares.get(msg.creator).get(msg.trancheKey).sharesWithdrawn -
                before.tranches_shares.get(msg.creator).get(msg.trancheKey).sharesWithdrawn
            
        }

    
    pure def withdrawLimitOrder(state: State, msg: WithdrawLimitOrderMsg) : State =
        // TODO: unfinished
        state 

    pure def placeLimitOrderPostcondition(before: State, msg: PlaceLimitOrderMsg, after: State) : bool =
        and{
            placeLimitOrderMatchedPostcondition(before, msg, after),
            placeLimitMakerOrderPostcondition(before, msg, after)
        }  

    // 
    pure def placeLimitOrderMatchedPostcondition(before: State, msg: PlaceLimitOrderMsg, after: State) : bool =
        true

    // This is a postcondition only for the part of placing an order that is a maker order, creating a new tranche
    pure def placeLimitMakerOrderPostcondition(
        before: State, 
        msg: PlaceLimitOrderMsg,
        after: State) : bool =

        
        val trancheIndex = calculateTrancheIndex(msg.tokenPair, msg.tick, before.tranches.keys())
        val trancheKey = (msg.tokenPair, msg.tick, trancheIndex)
        val amountIn = computeMakerOrderMatches(before, msg)._3
        and {
            // the tranche key exists in the after state
            after.tranches.keys().contains(trancheKey),
        
            // The share account per user and the tranche is there, and its values are updated correctly
            val userAccountBefore = before.tranches_shares.get(msg.creator)
            val userAccounotAfter = after.tranches_shares.get(msg.creator)
            
            and {
                userAccounotAfter.keys().contains(trancheKey),
                // the sharesOwned is increased: the split is depending on whether the user had already something in the
                // tranche or not
                userAccounotAfter.get(trancheKey).sharesOwned ==
                    if (userAccountBefore.keys().contains(trancheKey)) {
                        userAccountBefore.get(trancheKey).sharesOwned + amountIn
                    } else {
                        amountIn
                    },
                
                val increasedTrancheMaker =
                    // the difference in value of totalMaker in the pre and post state. 
                    // The split is based on whether the tranche existed before or was newly created
                    if (before.tranches.keys().contains(trancheKey)) {
                        
                        after.tranches.get(trancheKey).totalMaker - before.tranches.get(trancheKey).totalMaker
                    } else {
                        // just what is in the after state
                        after.tranches.get(trancheKey).totalMaker
                    }
                    increasedTrancheMaker == amountIn
            }   
        }
        
        

    action placeLimitOrderAction: bool =
        nondet c = oneOf(CREATORS)
        nondet o = oneOf(ORDER_TYPES)
        nondet t = oneOf(TICKS)
        nondet token_pair = oneOf(TOKEN_PAIRS)        
        // @audit-info
        // with this choice, we are not modelling user's funds. The assumption is that the user has as much as needed.
        // This is a simplification that we can revisit later.
        nondet amount = oneOf(1.to(MAX_AMOUNT))
        val limitOrderMsg = {
            creator: c,
            orderType: o,
            tick: t,
            tokenPair: token_pair,
            amountIn: amount
        }
        val res = placeLimitOrder(state, limitOrderMsg)

        all {
            state' = res,
            postViolation' = placeLimitOrderPostcondition(state, limitOrderMsg, res)
        }



    action cancelLimitOrderAction: bool =
        nondet creator = oneOf(state.tranches_shares.keys())
        nondet trancheKey = oneOf(state.tranches_shares.get(creator).keys())
        val cancelLimitOrderMsg = {
            creator: creator,
            trancheKey: trancheKey
        }
        val res = cancelLimitOrder(state, cancelLimitOrderMsg)
        all{
            state' = res,
            postViolation' = cancelLimitOrderPostcondition(state, cancelLimitOrderMsg, res)
        }

    
    action withdrawLimitOrderAction: bool = 
        nondet creator = oneOf(state.tranches_shares.keys())
        nondet trancheKey = oneOf(state.tranches_shares.get(creator).keys())
        val withdrawLimitOrderMsg = {
            creator: creator,
            trancheKey: trancheKey
        }
        val res = withdrawLimitOrder(state, withdrawLimitOrderMsg)
        all {
            require(state.tranches.keys().contains(trancheKey)),
            // we are not allowing  withdrawing from fresh tranches (there was no trading)
            // @audit-info Should we handle the error at this level or should we let individual functions handle it?
            // This way is cleaner. The problem is that the simulation will not try at all then those cases where a withdraw on a non-fresh
            // tranche is attempted.
            // maybe it makes sense to use a `require` here at places where we can be confident that the code will enforce those assumptions
            require(not(state.tranches.get(trancheKey).isFresh)),
            state' = res,
            postViolation' = withdrawLimitOrderPostcondition(state, withdrawLimitOrderMsg, res)
        }
    
    action advanceSingleBlockAction: bool =
        state' = {
            blockNumber: state.blockNumber + 1,
            ... state
        }

    action advanceMultipleBlocksAction: bool =
        nondet timestep = oneOf(1.to(MAX_FUTURE_BLOCKS))
        state' = {
            blockNumber: state.blockNumber + 2,
            ... state
        }

    action advanceTimeAction: bool = 
        all{
            // separating the two (even though they could be combined) to
            // be able to explicitly turn off one of them
            any {
                advanceSingleBlockAction,
                advanceMultipleBlocksAction
            },
            postViolation' = postViolation
        }        
        
        

    action step: bool = any {
        placeLimitOrderAction,

        cancelLimitOrderAction,

        withdrawLimitOrderAction,

        advanceTimeAction

    }



}