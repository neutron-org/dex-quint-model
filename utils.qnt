// -*- mode: Bluespec; -*-

module utils {
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"

    // This is a readability helper function: to put "require" around boolean conditions
    pure def require(cond: bool): bool = cond

    pure def min(a,b) = if(a<b) a else b

    pure def priceFromTick(tick: int) : Dec =
        BASE_PRICE.pow(-tick)

    pure def tokenLT(t1: Token, t2: Token, token_num_values: Token -> int) : bool =
        // @audit-info What is the best practice to encode assumptions on pure functions,
        // without making the code too verbose?
        // assuming that both tokens are in the TOKEN_NUM_VALUES
        token_num_values.get(t1) < token_num_values.get(t2)

    pure def orderTokenPair(pair: (Token, Token)): (Token, Token) =
      if (tokenLT(pair._1, pair._2, TOKEN_NUM_VALUES)) pair else (pair._2, pair._1)
        
    pure def normalizeCoinAndTick(tokenPair: (Token, Token), tick: int) : (Token, Token, int) =
        val tokenIn = tokenPair._1
        val tokenOut = tokenPair._2
        if (tokenLT(tokenIn, tokenOut, TOKEN_NUM_VALUES)) 
            (tokenIn, tokenOut, tick)
        else 
            (tokenOut, tokenIn, -tick) 
    }