// -*- mode: Bluespec; -*-
module types {
    import dec.* from "lib/dec"

    type Addr = str
    type TokenPair = (Token, Token)
    type TickIndex = int
    type Fee = int
    // a tranche index is there for multiple tranches of the same tick
    type TrancheIndex = int

    type TrancheKey = {
        makerToken: Token,
        takerToken: Token,
        tick: TickIndex,
        id: TrancheIndex
    }

    type Transition = {
        name: str,
        message: Message
    } 

    
    type Option[a] = None | Some(a)
    
    type LimitOrderType = 
        | FillOrKill
        | ImmediateOrCancel
        | GoodTillCancelled
        | GoodTillTime(BlockTime)
        | JustInTime

    // MESSAGES

    type Message = 
    | DepositPoolSubType(DepositMsg)
    | PlaceLimitOrderSubType(PlaceLimitOrderMsg)
    | CancelLimitOrderSubType(CancelLimitOrderMsg)
    | WithdrawLimitOrderSubType(WithdrawLimitOrderMsg)
    | WithdrawPoolSubType(WithdrawPoolMsg)
    | SwapSubType(SwapMsg)
    | Init
    | AdvanceTime



    //@audit-info : Simplified to only allow deposit in a single position
    type DepositMsg = {
            creator: Addr,
            tokenPair: (Token, Token),
            amountPair: (int, int),
            tick: TickIndex,
            fee: Fee,
            autoswap: bool
        }

    type WithdrawPoolMsg = {
            creator: Addr,
            tokenPair: (Token, Token),
            amount: int,
            tick: TickIndex,
            fee: Fee,
        }

    type SwapMsg = {
            creator: Addr,
            tokenPair: (Token, Token),
            amount: int,
            limitPriceTick: TickIndex
        }

    type PlaceLimitOrderMsg = {
            creator: Addr,
            orderType: LimitOrderType,            
            // we have to choose tickIndex because the log operation is missing
            // @audit-info : If we will really need the log operation, what are our Quint options?
            tick: TickIndex, 
            // semantics: I want to exchange tokenIn for tokenOut
            tokenIn: Token,
            tokenOut: Token,
            amountIn: int,
            maxAmountOut: int,
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
    // @audit-info We could model it as an option type. However, I have a feeling it would increase
    // boilerplate without clear wins. Am I wrong here?
    type BlockTime = int

    // @audit-info Is there a way to factor out the two types below? So that
    // Liquidity = LimitOrderTranche | PoolReserves, and then they are defined elsewhere?
    // I tried, but the checker complained about it.
    type Liquidity = 
        | LimitOrderTranche(Tranche)                
        // @audit-info merge this in the merging session with Manu
        | PoolReserves

    type LiquidityKey = TrancheLiquidityKey(TrancheKey) | PoolLiquidityKey(PoolId)
    type Tranche = {
            // does not exist in the code: here, we want to be explicit about it.
            // should only be true until a first exchange happens through the tranche
            // isFresh <=> reservesMaker == totalMaker
            isFresh: bool,
            reservesMaker: int,
            reservesTaker: int,
            totalMaker: int,
            totalTaker: int,
            // working directly with the tick instead of the `PriceTakerToMaker`
            tick: int,
            expirationTime: BlockTime,
            trancheKey: TrancheKey,
            // @audit-issue This is saved in the code under TrancheUser. Is there a good reason
            // for why this could not be udner tranche?
            makerDenomination: Token,
            takerDenomination: Token
        }

    
    type PoolId = {
        tokenPair: (Token, Token),
        tick: TickIndex,
        fee: Fee
    }

    type Pool = {
        reserves: (int, int),
        shares: int
    }

    type TrancheUpdateDiff = {makerReservesDiff: int, takerReservesDiff: int}
    type PoolUpdateDiff = {token0Diff: int, token1Diff: int}

    // @audit-issue We must make sure that `Shares` always contains all the Addr (Addr is not removed and is initialized)
    type UsersTrancheShares = {
        sharesOwned: int,
        sharesWithdrawn: int
    }
    type Shares = Addr -> ShareAccount
    type ShareAccount = TrancheKey -> UsersTrancheShares

    type SwapUpdateInfo = {
        remainingAmount: int,
        outAmount: int, 
        tranchesUpdates: TrancheKey -> TrancheUpdateDiff, 
        poolUpdates: PoolId -> PoolUpdateDiff        
        }

    type OrderedList[_a] = List[_a]

    type State = {
        
        tranches: TrancheKey -> Tranche,        
        tranchesShares: Shares,        

        poolShares: Addr -> PoolId -> int,
        pools: PoolId -> Pool, 

        // balances of all the users in the system
        coins: Coins,

        // we will model time by blocks (integer value). 
        // In the code, it is done through BlockTime. Departing from that. 
        // @audit-info could that cause any issue down the line?
        blockNumber: BlockTime,       

        // helper state variables, to be used in invariants
        deposits: Addr -> PoolId -> {amounts: (int, int), swaps: bool},
        poolWithdrawns: Addr -> PoolId -> (int, int)
    }

    type Result = {state: State, error: str}
}