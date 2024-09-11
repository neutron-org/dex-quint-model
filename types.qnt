// -*- mode: Bluespec; -*-
module types {
    import dec.* from "lib/dec"

    type Addr = str
    type TokenPair = (Token, Token)
    type TickIndex = int
    // a tranche index is there for multiple tranches of the same tick
    type TrancheIndex = int
    // each trancheId is a tuple of ((tokenA, tokenB), tick, trancheIndex)
    type TrancheKey = (TokenPair, TickIndex, TrancheIndex)
    
    
    type LimitOrderType = 
        | GoodTillCancelled
        | GoodTillTime(int)
        // TODO: more types still need to be added

    // MESSAGES

    type PlaceLimitOrderMsg = {
            creator: Addr,
            orderType: LimitOrderType,            
            // we have to choose tickIndex because the log operation is missing
            // @audit-info : If we will really need log, what are our Quint options?
            tick: TickIndex,
            tokenPair: (Token, Token)
        }
        
    type CancelLimitOrderMsg = {
            creator: Addr,
            trancheKey: TrancheKey
        }
    type WithdrawLimitOrderMsg = {
            creator: Addr,
            trancheKey: TrancheKey
        }

    // Coins captures the balances of all the users in the system
    type Coins = Addr -> TokenAccount
    type TokenAccount = Token -> int
    type Token = str
    // we model Time as int. The special value -1 will denote that the time is None
    // @audit-info: We could model it as an option type. However, I have a feeling it would increase
    // boilerplate without clear winse. Am I wrong here?
    type Time = int


    type Tranche = {
        // does not exist in the code: here, we want to be explicit about it.
        // should only be true until a first exchange happens through the tranche
        // isFresh <=> reservesMaker == totalMaker
        isFresh: bool,
        reservesMaker: Dec,
        reservesTaker: Dec,
        totalMaker: Dec,
        totalTaker: Dec,
        // working directly with the tick instead of the `PriceTakerToMaker`
        tick: int,
        expirationTime: Time

    }

    type Shares = Addr -> ShareAccount
    type ShareAccount = TrancheKey -> int

    type OrderedList[_a] = List[_a]

    type State = {
        // 
        tranches: TrancheKey -> Tranche,
        // balances of all the users in the system
        coins: Coins,
        shares: Shares,

        // We will need to iterate IN ORDER over the ticks corresponding to the same token pair
        // @audit-info I want to use the type to inform the reader here. Is there a way to use it also
        // to enforce the order in the model?
        // @audit-info Would it make sense to separate the state into "actual info" and
        // helper data structures (like the OrderedLists below)?        
        orderedTicks: TokenPair -> OrderedList[TickIndex],
        orderedTranchesPerTick: (TokenPair, TickIndex) -> OrderedList[TrancheKey]


        // ... and then we'll need data structures for pools and deposits
    }

}