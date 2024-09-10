// -*- mode: Bluespec; -*-

/**
 * A specification of the Duality DEX module at v0.5.0:
 * https://github.com/duality-labs/duality/tree/v0.5.0/x/dex
 *
 * Andrey Kuprianov, Informal Systems, 2023
 */

/* Design Principles

1. Error handling only at top level
   
   In the code, multiple functions may return errors when input is wrong.
   A good example of that is  `CalcPrice(relativeTickIndex int64) (math_utils.PrecDec, error)`:
   it returns an error when `relativeTickIndex` is out of range.

   In the model, handling errors at lower levels makes the model very clumsy.
   Therefore, we shift all error handling to the uppermost level (state updates);
   lower-level functions operate under the explicitely written assumptions of good inputs:

   pure def CalcPrice(relativeTickIndex: Int64): Dec = 
       // assume not IsTickOutOfRange(relativeTickIndex)
       ...



*/

module dexV5 {

import intN(BITWIDTH=8).* from "intN"
type Int64 = IntN

import uintN(BITWIDTH=8) as uint64 from "uintN"
type Uint64 = uint64::UintN

import dec.* from "dec"

type Token = str

// a pair of tokens, as a list for efficiency
type PairID = List[Token] 

type TradePairID = {
	makerDenom: Token,
    takerDenom: Token
}

pure def Reversed(p: TradePairID): TradePairID =
    {   makerDenom: p.takerDenom,
        takerDenom: p.makerDenom
    }

pure def NewTradePairIDFromMaker(pairID: PairID, makerDenom: Token): TradePairID = 
	{   takerDenom: if(pairID[0] == makerDenom) pairID[1] else pairID[0],
		makerDenom: makerDenom
	}

type PoolReservesKey = {
	tradePairID: TradePairID, 
    tickIndexTakerToMaker: Int64,
    fee: Uint64
}

type PoolReserves = {
    key: PoolReservesKey,
    reservesMakerDenom: int,
    priceTakerToMaker: Dec,
    priceOppositeTakerToMaker: Dec
}

type Pool = {
    id: Uint64,
    lowerTick0: PoolReserves,
    upperTick1: PoolReserves
}

pure def MaxTickExp: Uint64 = 559_680
pure def BasePrice = fromRatio(10001, 10000)

pure def IsTickOutOfRange(tickIndex: Int64): bool =
    tickIndex > 0 and uint64::toUint(tickIndex) > MaxTickExp

pure def CalcPrice(relativeTickIndex: Int64): Dec = 
    // assume not IsTickOutOfRange(relativeTickIndex)
    if(relativeTickIndex < 0)
        BasePrice.pow(uint64::toUint(-1.wrapping_mul(relativeTickIndex)))
    else
        one.div(BasePrice.pow(uint64::toUint(relativeTickIndex)))

pure def PriceTakerToMaker(p: PoolReservesKey): Dec = 
	// assume not IsTickOutOfRange(p.tickIndexTakerToMaker)
    CalcPrice(p.tickIndexTakerToMaker)

pure def Counterpart(p: PoolReservesKey): PoolReservesKey =
    // assume p.fee <= int64::Max
    val feeInt64 = toInt(p.fee)
	{   tradePairID: p.tradePairID.Reversed(),
		tickIndexTakerToMaker: 
            p.tickIndexTakerToMaker.wrapping_mul(-1).wrapping_add(2.wrapping_mul(feeInt64)),
		fee: p.fee
	}

pure def NewPoolReserves(poolReservesID: PoolReservesKey): PoolReserves =
	// assume poolReservesID.fee <= int64::Max
    // assume not IsTickOutOfRange(poolReservesID.tickIndexTakerToMaker)
    // assume not IsTickOutOfRange(poolReservesID.Counterpart().tickIndexTakerToMaker)
    {   key: poolReservesID,
        reservesMakerDenom: 0,
        priceTakerToMaker: poolReservesID.PriceTakerToMaker(),
        priceOppositeTakerToMaker: poolReservesID.Counterpart().PriceTakerToMaker()
    }

pure def NewPoolReservesFromCounterpart(counterpart: PoolReserves): PoolReserves =
     // assume counterpart.fee <= int64::Max
    {   key: counterpart.key.Counterpart(),
		reservesMakerDenom: 0,
		priceTakerToMaker: counterpart.priceOppositeTakerToMaker,
		priceOppositeTakerToMaker: counterpart.priceTakerToMaker,
	}


pure def NewPool(pairID: PairID, centerTickIndexNormalized: Int64, fee: Uint64, id: Uint64): Pool = 
    // assume fee <= int64::Max
    // assume not IsTickOutOfRange(centerTickIndexNormalized.wrapping_add(toInt(fee)))
    val feeInt64 = toInt(fee)
    val id0To1 = {
		tradePairID:    NewTradePairIDFromMaker(pairID, pairID[1]),
		tickIndexTakerToMaker: centerTickIndexNormalized.wrapping_add(feeInt64),
		fee: fee
	}
    val upperTick = NewPoolReserves(id0To1)
    val lowerTick = NewPoolReservesFromCounterpart(upperTick)
    {   id: id,
        lowerTick0: lowerTick,
        upperTick1: upperTick
    }


}
