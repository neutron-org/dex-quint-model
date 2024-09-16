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

    pure def computeWithdrawAmountMakerToken(percentFilled: Dec, userNonWithdrawnTrancheShare: Dec): int =
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
    
    pure def computeCancelAmountTakerToken(percentFilled: Dec, userNonWithdrawnTrancheShare: Dec, totalTaker: int): int = 
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
        // @audit-info I am using "MakerToken" instead of "TokenIn". Is there a good reason to stick to "TokenIn" and "TokenOut" language?
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
                computeCancelAmountTakerToken(percentFilled, userNonWithdrawnTrancheShare, tranche_before.totalTaker)

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
        nondet creator = oneOf(state.tranches_shares.keys())
        nondet trancheKey = oneOf(state.tranches_shares.get(creator).keys())
        val cancelLimitOrderMsg = {
            creator: creator,
            trancheKey: trancheKey
        }
        state' = cancelLimitOrder(state, cancelLimitOrderMsg)

    
    action withdrawLimitOrderAction: bool = 
        nondet creator = oneOf(state.tranches_shares.keys())
        nondet trancheKey = oneOf(state.tranches_shares.get(creator).keys())
        val withdrawLimitOrderMsg = {
            creator: creator,
            trancheKey: trancheKey
        }
        all {
            require(state.tranches.keys().contains(trancheKey)),
            require(not(state.tranches.get(trancheKey).isFresh)),
            state' = withdrawLimitOrder(state, withdrawLimitOrderMsg)
        }
    
    action advanceSingleBlockAction: bool =
        state' = {
            blockNumber: state.blockNumber + 1,
            ... state
        }

    action advanceMultipleBlocksAction: bool =
        nondet t = oneOf(1.to(MAX_FUTURE_BLOCKS))
        state' = {
            blockNumber: state.blockNumber + t,
            ... state
        }

    action advanceTimeAction: bool = 
        // separating the two (even though they could be combined) to
        // be able to explicitly turn off one of them
        any {
            advanceSingleBlockAction,
            advanceMultipleBlocksAction
        }        
        

    action step: bool = any {
        placeLimitOrderAction,

        cancelLimitOrderAction,

        withdrawLimitOrderAction,

        advanceTimeAction

    }



    }