module dexV5funcs {

import intN(BITWIDTH=8).* from "intN"
type Int64 = IntN

import uintN(BITWIDTH=8) as uint64 from "uintN"
type Uint64 = uint64::UintN

import dec.* from "dec"

// const MaxTickExp uint64 = 559_680
pure def MaxTickExp: Uint64 = 559_680

/*
func CalcFee(upperTickIndex, lowerTickIndex int64) int64 {
	return (upperTickIndex - lowerTickIndex) / 2
}
*/    
pure def CalcFee(upperTick: Int64, lowerTick: Int64): Int64 =
    upperTick.wrapping_sub(lowerTick)

/*
func IsTickOutOfRange(tickIndex int64) bool {
	return tickIndex > 0 && uint64(tickIndex) > MaxTickExp
}
*/
pure def IsTickOutOfRange(tickIndex: Int64): bool =
    tickIndex > 0 and uint64::toUint(tickIndex) > MaxTickExp

/*
func BasePrice() math_utils.PrecDec {
	return math_utils.MustNewPrecDecFromStr("1.0001")
}
*/
pure def BasePrice = fromRatio(10001, 10000)

/*
func CalcPrice(relativeTickIndex int64) (math_utils.PrecDec, error) {
	if IsTickOutOfRange(relativeTickIndex) {
		return math_utils.ZeroPrecDec(), ErrTickOutsideRange
	}
	if relativeTickIndex < 0 {
		return utils.BasePrice().Power(uint64(-1 * relativeTickIndex)), nil
	} else {
		return math_utils.OnePrecDec().Quo(utils.BasePrice().Power(uint64(relativeTickIndex))), nil
	}
}
*/

type Error = str
pure def ErrTickOutsideRange = "ErrTickOutsideRange"
pure def OK = "ok"

pure def CalcPrice(relativeTickIndex: Int64): (Dec, Error) = 
    if(IsTickOutOfRange(relativeTickIndex))
        (zero, ErrTickOutsideRange)
    else
        if(relativeTickIndex < 0)
            (BasePrice.pow(uint64::toUint(-1.wrapping_mul(relativeTickIndex))), OK)
        else
            (one.div(BasePrice.pow(uint64::toUint(relativeTickIndex))), OK)

/*
func CalcResidualValue(
	amount0, amount1 sdk.Int,
	priceLowerTakerToMaker math_utils.PrecDec,
	fee int64,
) (math_utils.PrecDec, error) {
	// ResidualValue = Amount0 * (Price1to0Center / Price1to0Upper) + Amount1 * Price1to0Lower
	amount0Discount, err := CalcPrice(-fee)
	if err != nil {
		return math_utils.ZeroPrecDec(), err
	}

	return amount0Discount.MulInt(amount0).Add(priceLowerTakerToMaker.MulInt(amount1)), nil
}
*/

pure def CalcResidualValue(amount0: int, amount1: int, priceLowerTakerToMaker: Dec, fee: Int64)
    : (Dec, Error) =
    val amount0DiscountErr = CalcPrice(-1.wrapping_mul(fee))
    if(amount0DiscountErr._2 != OK)
        (zero, amount0DiscountErr._2)
    else 
        (amount0DiscountErr._1.mulInt(amount0).add(priceLowerTakerToMaker.mulInt(amount1)), OK)

/*
func (p *Pool) CalcResidualSharesMinted(
	residualAmount0 sdk.Int,
	residualAmount1 sdk.Int,
) (sharesMinted sdk.Coin, err error) {
	fee := CalcFee(p.UpperTick1.Key.TickIndexTakerToMaker, p.LowerTick0.Key.TickIndexTakerToMaker)
	valueMintedToken0, err := CalcResidualValue(
		residualAmount0,
		residualAmount1,
		p.LowerTick0.PriceTakerToMaker,
		fee,
	)
	if err != nil {
		return sdk.Coin{Denom: p.GetPoolDenom()}, err
	}

	return sdk.Coin{Denom: p.GetPoolDenom(), Amount: valueMintedToken0.TruncateInt()}, nil
}
*/

// pure def CalcResidualSharesMinted(residualAmount0: int, residualAmount1: int, 
//     UpperTickpriceLowerTakerToMaker: Dec)
//     : (int, Error) =
//     val fee = CalcFee()


}