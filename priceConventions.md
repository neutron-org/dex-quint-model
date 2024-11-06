# Price Conventions

Since there were changes to price conventions, spanning documentation, specification, and the code, and we were changing the conventions mid developing the model, I am explicitly outlining them in this document.

## Vocabulary
We will be using the notion of a _sell price_. 
If token `A` has sell price `p` (for its counterparty `B`), this means that one `A` is worth `p` `B`s.
In the context of selling, this means that we are ready to sell one `A` for `p` `B`s.

_Tick_ is the integer representation of the price. 
A price for tick `t` is calculated as `1.0001^t`.

## Pools
Pools store liquidity provided in deposits. 
Pools are stored under pool keys of the form `(pair, tick, fee)`
 - pair: a pair will always be sorted alphabetically, e.g., `(A, B)`
 - tick: integer representation of the price
 - fee: additional fee that the buyer pays

 By convention, the selling price of token `A` is `1.0001^t + fee`, and the selling price of token `B` is `1.0001^-t + fee`. Excluding fees, these prices are exactly reversed.

The structure `DepositMsg` contains field `token0SellTick`: this is the tick `t` from the above explanation. The name `token0...` emphasises the convention that the tick will be used for the first token in the pair.


 ## Tranches
 Tranches store liquidity as obtained by placed limit orders.
 Tranches are stored under keys of the form
 ```
 type TrancheKey = {
        makerToken: Token,
        takerToken: Token,
        sellTick: TickIndex,
        id: TrancheIndex
    }
 ```
 - `makerToken`: the token that is offered as liquidity
 - `takerToken`: the counterparty token
 - `sellTick`: a tick `t` corresponding to the selling price of the makerToken
 - `id` : unique id

 The message `PlaceLimitOrderMsg` contains the field `sellTick`.
 This is the tick from above.
 (Note, limit orders have a dual purpose: one one hand, they are used for exchanging, and the rest amount is used to place new liquidity. We could have equally used a different convention.)

 ## Swaps
 Unlike tranches or pools, swaps do not provide liquidity.
 The `SwapMsg` looks like this
 
 ```
 type SwapMsg = {
    creator: Addr,
    tokenPair: (Token, Token),
    amount: int,
    limitPriceTick: TickIndex
    }
```

The `tokenPair` is a pair of the tokenIn (token that we want to swap) and tokenOut (token that we want to get in exchange).
The 
