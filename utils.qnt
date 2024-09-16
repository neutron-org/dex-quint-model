// -*- mode: Bluespec; -*-

module utils {
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"

    // This is a readability helper function: to put "require" around boolean conditions
    pure def require(cond: bool): bool = cond

    // TRANCHE related functions
    pure def calculatePercentFilled(t: Tranche) : Dec =
        fromRatio(t.totalMaker - t.reservesMaker, t.totalMaker)
    
    pure def calculatePercentUnfilled(t: Tranche) : Dec =
        fromRatio(t.reservesMaker, t.totalMaker)
    
    pure def calculateUserTotalTrancheShare(t: Tranche, users_share_account: ShareAccount) : Dec =
        val userShare = users_share_account.get(t.trancheKey).sharesOwned
        fromRatio(userShare, t.totalMaker)

    pure def calculateUserNonWithdrawnTrancheShare(t: Tranche, users_share_account: ShareAccount) : Dec =
        val userShare = users_share_account.get(t.trancheKey).sharesOwned - users_share_account.get(t.trancheKey).sharesWithdrawn
        fromRatio(userShare, t.totalMaker)


    // -------
    
    pure def priceFromTick(tick: int) : Dec =
        BASE_PRICE.pow(-tick)


    pure def tokenLT(t1: Token, t2: Token, token_num_values: Token -> int) : bool =
        // @audit-info What is the best practice to encode assumptions on pure functions,
        // without making the code too verbose?
        // assuming that both tokens are in the TOKEN_NUM_VALUES
        token_num_values.get(t1) < token_num_values.get(t2)


    
    pure def normalizeCoinAndTick(tokenPair: (Token, Token), tick: int) : ((Token, Token), int) =
        val tokenIn = tokenPair._1
        val tokenOut = tokenPair._2
        if (tokenLT(tokenIn, tokenOut, TOKEN_NUM_VALUES)) 
            ((tokenIn, tokenOut), tick)
        else 
            ((tokenOut, tokenIn), -tick)         
    
    
    
    pure def calculateTrancheIndex(tokenPair: TokenPair, tick: TickIndex, existingTranchesKeys: Set[TrancheKey]): int = 
    
        // TODO: NOT-IMPLEMENTED
        -1
    
    }

    