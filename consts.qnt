// -*- mode: Bluespec; -*-
module consts {
    import types.* from "types"
    import dec.* from "lib/dec"

    //  Helper vals
    pure val CREATORS: Set[Addr] = Set("Alice", "Bob", "Carol")
    pure val INITIAL_CREATORS_BALANCES: Set[int] = Set(1000, 500)
    pure val INITIAL_CREATOR_BALANCE_TEMP: int = 1000
    // pure val ORDER_TYPES: Set[LimitOrderType] = Set(GoodTillCancelled, ImmediateOrCancel, FillOrKill, JustInTime, GoodTillTime(-1))
    pure val ORDER_TYPES: Set[LimitOrderType] = Set(GoodTillCancelled)
    pure val PRICES: Set[int] = Set(10000000, 11500000, 9500000)
    pure val AMOUNTS: Set[int] = Set(5, 10, 15, 20)
    
    pure val TOKENS: Set[Token] = Set("A", "B", "C")
    pure val TOKEN_PAIRS: Set[TokenPair] = tuples(TOKENS, TOKENS).filter(x => x._1 != x._2)
    pure val TOKEN_NUM_VALUES: Token -> int = Map("A" -> 0, "B" -> 1, "C" -> 2)
    pure val BASE_PRICE: Dec = (10001, 10000)
    // We will use this value to denote how much we can advance time and (at the same time!) the maximum
    // good_til value for a limit order
    pure val MAX_FUTURE_BLOCKS: int = 10

    val MAX_TICK = 100000
    val MAX_SIMULATION_TICK = 10
    val MAX_FEE = 10
    val TICKS: Set[TickIndex] = (-MAX_SIMULATION_TICK).to(MAX_SIMULATION_TICK)
    val MAX_AMOUNT = 100
    }