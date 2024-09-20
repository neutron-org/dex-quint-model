# Neutron DEX Model
This repository is a work-in-progress specification of the Neutron DEX module. 

- Ongoing modeling: limit orders and their interactions :hourglass:

## Repository Structure

 - `lib`: generally useful Quint functions, for modelling arithmetic operatoins
 - `history`: previous, related models (developed in 2023 during earlier audits), some of them unfinished
 - `.`: the files that make the model in progress

## Specification invariants

### Limit Orders

**LO.A:** When limit orders are swapped through the pro-rata portion of the swap value value will returnable to all participants via withdraws

  1. trancheUser.SharesOwned = amountIn
  2. totalMakerDenom = sum(allTrancheUsers.SharesOwned)
  3. proRataShare = trancheUser.SharesOwned / totalMakerDenom
  4. percentFilled = reservesTakerDenom * price /  totalMakerDenom
  5. withdrawAmount = proRataShare * percentFilled * reservesTakerDenom - sharesWithdrawn
  6. note: withdraws can happen at any time and any number of times throughout the lifecycle of a limit order.

**LO.B:** When canceling a limit order the participants will receive their pro-rata share of any unused maker denom as well as their pro-rata share of all swap proceeds. Cancellations will not change the future withdraw or cancel outcomes for other participants.

  1. cancelAmountOutTokenIn = (1-percentFilled) * proRataShare * totalMakerDenom
  2. cancelAmountOutTokenOut = proRataShare * percentFilled * reservesTakerDenom - SharesWithdrawn

### Liquidity Pools

**LP.A:** Shares are correctly minted when depositing into a pool.

1. shareValue = totalPreexistingShares / totalPoolValue
2. depositValue = depositAmountToken0 + depositAmountToken1 * price
3. sharesMinted = depositValue * shareValue

**LP.B:** Autoswap correctly adjusts shares minted.

1. autoswapFee = residualValue0 * poolFee + residualValue1 * poolFee * price
2. shareValue = totalPreexistingShares / (totalPoolValue + autswapFee)
3. sharesMinted = (depositValue - autoswapFee) * shareValue

**LP.C:** Swapping through a pool can only increase the total value of the pool (assuming token price is the tick price)
1. preexistingPoolValue = reserves0 + reserves1 * price
2. totalPoolValue = preexistingPoolValue + swapAmountToken0 * (1 + fee) + swapAmountToken1 * (1 + fee) * price
   
**LP.D:** Withdrawing from a pool will yield the correct pro-rata value based on the number of shares withdrawn
1. proRataShare = sharesOwned / totalShares
2. withdrawAmount0 = proRataShare * poolReserves0
3. withdrawAmount1 = proRataShare * poolReserves1

**LP.E:** When a single user withdraws 100% of their shares from the pool

1. For each time period t when a swap occurs a user receives the correct proportion of the swap fee
2. depositValue = token0 + token1 * price
3. swapFee = token0In * poolFee + token1In  * poolFee * price
4. poolValue = reserves0 + reserves1 * price