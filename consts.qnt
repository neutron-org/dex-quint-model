// -*- mode: Bluespec; -*-
module consts {
    import types.* from "types"
    import dec.* from "lib/dec"

    //  Helper vals
    val CREATORS: Set[Addr] = Set("Alice", "Bob", "Carol")
    val ORDER_TYPES: Set[LimitOrderType] = Set(GoodTillCancelled)
    val PRICES: Set[int] = Set(10000000, 11500000, 9500000)
    val AMOUNTS: Set[int] = Set(5, 10, 15, 20)
    val TOKEN_PAIRS: Set[TokenPair] = Set(("A", "B"), ("B", "A"))
    val TOKEN_NUM_VALUES: Token -> int = Map("A" -> 0, "B" -> 1)
    val TICKS: Set[TickIndex] = Set(4)
    pure val BASE_PRICE: Dec = (10001, 10000)
    }