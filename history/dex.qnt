// -*- mode: Bluespec; -*-

/**
 * A specification of the Duality DEX module at v0.2.0:
 * https://github.com/duality-labs/duality/tree/v0.2.0/x/dex
 *
 * Andrey Kuprianov, Informal Systems, 2023
 */
module Dex {

import Dec.* from "dec"
import Util.* from "util"

// Constants
pure val TOKENS = Set("A", "B", "C", "D")
pure val TOKEN_ORDER = Map("A" -> 1, "B" -> 2, "C" -> 3, "D" -> 4)
pure val Module = "Module"
pure val ADDRESSES = Set(Module, "Alice", "Bob", "Carol", "Dave")
pure val MAX_AMOUNT = 1000
pure val MAX_TICK = 100000
pure val TICKS = (-MAX_TICK).to(MAX_TICK)
pure val AMOUNTS = 0.to(MAX_AMOUNT)
pure val BASE_PRICE: Dec = (10001, 10000) 

// Basic types
type TickIndex = int
type Token = string
type Addr = string
type TradingPair = List[Token]
type PoolID = { tokens: TradingPair, tick: TickIndex, fee: TickIndex }
type TokenAccount = Token -> int
type ShareAccount = PoolID -> int

// A pair of token amounts
type AmountPair = List[int]

type Pool = { reserves: AmountPair, shares: int }
type Liquidity = { tick: TickIndex, fee: TickIndex }

// Liquidity for a particular trading pair, ordered by the price (tick)
type LiquidityList = List[Liquidity]
type Liquidities = TradingPair -> LiquidityList

type Pools = PoolID -> Pool
type Coins = Addr -> TokenAccount
type Shares = Addr -> ShareAccount

type State = {
    pools: Pools,
    coins: Coins,
    shares: Shares,
    liquidity: Liquidities,
}

pure def initialState: State = {
    pools: Map(), coins: Map(),
    shares: Map(), liquidity: Map()
}

// Result of executing an operation on the state
type Result = { state: State, result: str }

pure def ok(s: State): Result = { state: s, result: "ok" }
pure def error(reason: str): Result = { state: initialState, result: reason }
pure def isOk(r: Result): bool = r.result == "ok"
pure def isError(r: Result): bool = not(r.isOk())

// Token order operations
pure def tokenBefore(a: Token, b: Token): bool = TOKEN_ORDER.get(a) <= TOKEN_ORDER.get(b) 
pure def isValidTradingPair(p: TradingPair): bool = p[0] != p[1]
pure def orderTradingPair(p: TradingPair): TradingPair =
    if(tokenBefore(p[0],p[1])) p else rev(p)
pure def isOrderedTradingPair(p: TradingPair): bool =
    orderTradingPair(p) == p

pure def addAmounts(p: AmountPair, a: AmountPair): AmountPair = [p[0] + a[0], p[1] + a[1]]
pure def subAmounts(p: AmountPair, a: AmountPair): AmountPair = [p[0] - a[0], p[1] - a[1]]

// Account operations
pure def addCoins(accounts: Addr -> t -> int, user: Addr, token: t, amount: int):  Addr -> t -> int = {
    val account = if(user.in(accounts.keys())) accounts.get(user) else Map()
    val newAmount = 
        if(token.in(account.keys())) account.get(token) + amount
        else amount
    accounts.put(user, account.put(token,newAmount))
}

pure def subCoins(accounts, user, token, amount) = 
    accounts.addCoins(user, token, -amount)

pure def transferCoins(accounts, sender, receiver, token, amount) = {
    accounts.subCoins(sender, token, amount)
            .addCoins(receiver, token, amount)
}

// Pool operations
pure def initPool: Pool = { reserves: [0, 0], shares: 0 } 

def getOrInitPool(pools: Pools, poolID: PoolID): Pool = {
    if(poolID.in(pools.keys())) pools.get(poolID) else initPool
}

pure def addReserves(pool: Pool, amounts: AmountPair): Pool = 
    { reserves: pool.reserves.addAmounts(amounts), ...pool }

pure def subReserves(pool: Pool, amounts: AmountPair): Pool = 
    pool.addReserves([-amounts[0], -amounts[1]])

pure def addShares(pool: Pool, amount: int): Pool = 
    { shares: pool.shares + amount, ... pool }

pure def subShares(pool: Pool, amount: int): Pool = 
    { shares: pool.shares - amount, ... pool }

// Liquidity operations
pure def cheaper(liqs: LiquidityList, tick: TickIndex, fee: TickIndex): LiquidityList = 
    liqs.select(l => l.tick < tick or (l.tick == tick and l.fee < fee))

pure def expensive(liqs: LiquidityList, tick: TickIndex, fee: TickIndex): LiquidityList = 
    liqs.select(l => l.tick > tick or (l.tick == tick and l.fee > fee))

pure def changePoolLiquidity(liqs: LiquidityList, reserve: int, tick: TickIndex, fee: TickIndex): LiquidityList =
    liqs.cheaper(tick, fee)
    .concat(if(reserve!=0) [{tick: tick, fee: fee}] else [])
    .concat(liqs.expensive(tick, fee))

pure def getLiquidityList(liqMap: Liquidities, tokens: TradingPair): LiquidityList =
    if(tokens.in(liqMap.keys())) liqMap.get(tokens)
    else []

pure def updatePoolLiquidity(liqMap: Liquidities, poolID: PoolID, pool: Pool): Liquidities =
    val upperLiqs = liqMap.getLiquidityList(poolID.tokens)
    val newUpperLiqs = upperLiqs.changePoolLiquidity(pool.reserves[0], poolID.tick+poolID.fee, poolID.fee)
    val lowerLiqs = liqMap.getLiquidityList(rev(poolID.tokens))
    val newLowerLiqs = lowerLiqs.changePoolLiquidity(pool.reserves[1], poolID.tick-poolID.fee, poolID.fee)
    liqMap.put(poolID.tokens, newUpperLiqs)
        .put(rev(poolID.tokens), newLowerLiqs)


// Deposit operations

pure def calcGreatestMatchingRatio(reserves: AmountPair, amounts: AmountPair): int = 
    if(reserves[1] > 0)
        min(amounts[0], amounts[1].dec().mul(reserves[0].dec()).div(reserves[1].dec()).truncate())
    else amounts[0]

pure def calcGreatestMatchingRatioPair(pool: Pool, amounts: AmountPair): AmountPair = [
    calcGreatestMatchingRatio(pool.reserves, amounts), 
    calcGreatestMatchingRatio(rev(pool.reserves), rev(amounts))
]

pure def calcAmountAsToken0(amounts: AmountPair, price1To0: Dec): Dec = {
    amounts[0].dec().add(price1To0.mul(amounts[1].dec()))
}

pure def calcSharesMinted(pool: Pool, amounts: AmountPair, tick: TickIndex): int = {
    val mintedToken0 = calcAmountAsToken0(amounts, BASE_PRICE.pow(-tick))
    val existingToken0 = calcAmountAsToken0(pool.reserves, BASE_PRICE.pow(-tick))
    if(existingToken0.gt(dec(0)))
        mintedToken0.mul(dec(pool.shares)).div(existingToken0).truncate()
    else
        mintedToken0.truncate()
}

// Deposit token amounts into a pool
pure def deposit(s: State, sender: Addr, receiver: Addr, 
    tokens: TradingPair, tick: TickIndex, fee: TickIndex, tokenAmounts: AmountPair): Result = 
    val pair = orderTradingPair(tokens)
    val amounts = if(pair == tokens) tokenAmounts else rev(tokenAmounts)
    val poolID = { tokens: pair, tick: tick, fee: fee }
    val pool = s.pools.getOrInitPool(poolID)
    val inAmounts = pool.calcGreatestMatchingRatioPair(amounts)
    val outShares = pool.calcSharesMinted(inAmounts, poolID.tick)
    val newPool = pool.addReserves(inAmounts).addShares(outShares)
    if(not(isValidTradingPair(poolID.tokens))) 
        error("Invalid trading pair")
    else ok({  
        pools: s.pools.put(poolID, newPool),
        coins: s.coins
                .transferCoins(sender, Module, pair[0], inAmounts[0])
                .transferCoins(sender, Module, pair[1], inAmounts[1]),
        shares: s.shares.addCoins(receiver, poolID, outShares),
        liquidity: s.liquidity.updatePoolLiquidity(poolID, newPool),
        ...s
    })


pure def redeemValue(pool: Pool, shares: int): AmountPair = {
    pure def outAmount(reserve) =
        (reserve * shares).dec().div(pool.shares.dec()).truncate()
    [outAmount(pool.reserves[0]), outAmount(pool.reserves[1])]
}

// Withdraw shares from a pool
pure def withdraw(s: State, sender: Addr, receiver: Addr, 
    tokens: TradingPair, tick: TickIndex, fee: TickIndex, shares: int): Result =
    val pair = orderTradingPair(tokens)
    val poolID = { tokens: pair, tick: tick, fee: fee }
    val pool = s.pools.getOrInitPool(poolID)
    val outAmounts = pool.redeemValue(shares)
    val newPool = pool.subReserves(outAmounts).subShares(shares)
    if(not(isValidTradingPair(poolID.tokens))) 
        error("Invalid trading pair")
    else if(shares > pool.shares)
        error("Not enough shares")
    else ok({  
        pools: s.pools.put(poolID, newPool),
        coins: s.coins
                .transferCoins(Module, receiver, pair[0], outAmounts[0])
                .transferCoins(Module, receiver, pair[1], outAmounts[1]),
        shares: s.shares.subCoins(sender, poolID, shares),
        liquidity: s.liquidity.updatePoolLiquidity(poolID, newPool),
        ...s
    })


// Price as a relative tick index, to convert Token0 to Token1
// Given amount0 of Token0, we get the following of Token1:
// amount1 = amount0 * 1.0001 ^ (-price)
// Notice that as the price increases, you get less of Token1
// for the same amount of Token0, i.e. the expected behavior wrt. prices
type Price = TickIndex

pure def mulDec(price: Price, amount: Dec): Dec = 
    amount.div(BASE_PRICE.pow(price))

pure def mulInt(price: Price, amount: int): Dec = 
    price.mulDec(dec(amount))

// Swap liquidity from Token0 (in) to Token1 (out), 
// given the max desired amounts, the available reserves of Token0 and Token1,
// as well as the price from Token0 to Token1
// Returns the changes in the reserves
pure def calcSwapAmounts(maxAmounts: AmountPair, reserves: AmountPair, price: Price): AmountPair = {
    val max1Given0 = price.mulInt(maxAmounts[0]).truncate()
    val outAmount = minList([max1Given0, maxAmounts[1], reserves[1]])
    val inAmount = 
        if(outAmount == max1Given0) maxAmounts[0]
        else (-price).mulInt(outAmount).ceil()
    [inAmount, -outAmount]
}

// Swap liquidity via the pool from Token0 (in) to Token1 (out),
// given the max desired amounts of Token0 and Token1, respectively
// Returns the updated pool and the amount changes from the pool perspective
pure def calcPoolSwap0To1(pool: Pool, maxAmounts: AmountPair, tick: TickIndex): (Pool, AmountPair) = {
    val amounts = calcSwapAmounts(maxAmounts, pool.reserves, tick) //pool.tick+pool.fee
    (pool.addReserves(amounts), amounts)
}

// Swap liquidity via the pool from Token1 (in) to Token0 (out),
// given the max desired amounts of Token1 and Token0, respectively
// Returns the updated pool and the amount changes from the pool perspective
pure def calcPoolSwap1To0(pool: Pool, maxAmounts: AmountPair, tick: TickIndex): (Pool, AmountPair) = {
    val amounts = calcSwapAmounts(maxAmounts, rev(pool.reserves), tick) //-(pool.tick-pool.fee))
    (pool.addReserves(rev(amounts)), amounts)
}

// Swaps liquidity of a single pool
// Returns the updated state, and the remaining (unswapped) amounts
pure def swapPoolLiquidity(s: State, tokens: TradingPair, maxAmounts: AmountPair, liq: Liquidity): (State, AmountPair) =
    val is0To1 = isOrderedTradingPair(tokens)
    val pairID = orderTradingPair(tokens)
    val centerTick = liq.tick + if(is0To1) -liq.fee else liq.fee
    val poolID = { tokens: pairID, tick: centerTick, fee: liq.fee }
    val pool = s.pools.get(poolID)
    val newPoolAndAmounts = 
        if(is0To1) calcPoolSwap0To1(pool, maxAmounts, liq.tick)
        else calcPoolSwap1To0(pool, maxAmounts, liq.tick)
    ({  pools: s.pools.put(poolID, newPoolAndAmounts._1),
        liquidity: s.liquidity.updatePoolLiquidity(poolID, newPoolAndAmounts._1),
        ...s},
        [maxAmounts[0]-newPoolAndAmounts._2[0], maxAmounts[1]+newPoolAndAmounts._2[1]]
    )

// Swap up to given amounts of tokens through all available liquidity pools
pure def swap(s: State, sender: Addr, receiver: Addr, tokens: TradingPair, amounts: AmountPair): Result =
    if(not(isValidTradingPair(tokens)))
        error("Invalid trading pair")
    else
        val liqs = s.liquidity.getLiquidityList(tokens)
        val newStateAndAmounts = liqs.foldl((s, amounts),
            (stateAndAmounts, liq) => 
                swapPoolLiquidity(stateAndAmounts._1, tokens, stateAndAmounts._2, liq)
        )
        val newState = newStateAndAmounts._1
        val swappedAmounts = amounts.subAmounts(newStateAndAmounts._2)
        ok({
            coins: newState.coins
                .transferCoins(sender, Module, tokens[0], swappedAmounts[0])
                .transferCoins(Module, receiver, tokens[1], swappedAmounts[1]),
            ...newState
        })


// Executing actions, and bookkeeping

pure def MESSAGES = Set("deposit", "withdraw", "swap")

// A join of all parameters from all actions
type Message = {
    kind: str,
    sender: Addr, 
    receiver: Addr, 
    tokens: TradingPair,
    tick: TickIndex,
    fee: TickIndex, 
    amounts: AmountPair,
    shares: int
}

pure def nondetMessage: Message = 
    nondet msg = {
        kind: oneOf(MESSAGES),
        sender: oneOf(ADDRESSES),
        receiver: oneOf(ADDRESSES),
        tokens: [oneOf(TOKENS), oneOf(TOKENS)], 
        tick: oneOf(TICKS), 
        fee: oneOf(TICKS),
        amounts: [oneOf(AMOUNTS), oneOf(AMOUNTS)],
        shares: oneOf(AMOUNTS)
    }
    msg

// State variables
var state: State
var history: List[(str, Message)]


action init: bool = all {
    state' = initialState,
    history' = []
}

action step: bool = {
    val msg = nondetMessage
    val res = 
        if(msg.kind == "deposit") 
            deposit(state, msg.sender, msg.receiver, 
                msg.tokens, msg.tick, msg.fee, msg.amounts)
        else if(msg.kind == "withdraw") 
            withdraw(state, msg.sender, msg.receiver, 
                msg.tokens, msg.tick, msg.fee, msg.shares)
        else if(msg.kind == "swap") 
            swap(state, msg.sender, msg.receiver, 
                msg.tokens, msg.amounts)
        else error("Wrong message type")
    all {
        history' = history.append((res.result, msg)),
        state' = 
            if(res.result == "ok") res.state
            else state
    }
}

} // Module Dex