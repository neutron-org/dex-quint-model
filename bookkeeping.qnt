// -*- mode: Bluespec; -*-
module bookkeeping {
    import basicSpells.* from "lib/basicSpells"
    import utils.* from "utils"
    import types.* from "types"

    pure def trackTrancheDeposit(t: TrackedValue, addr: Addr, key: Option[TrancheKey], amount: int): TrackedValue = {
        match key {
          | None => t
          | Some(key) => {
              pure val deposits = t.tranches.deposits
              pure val newDeposits =
                  if (deposits.has((addr, key)))
                      deposits.setBy((addr, key), v => v + amount)
                  else
                      deposits.put((addr, key), amount)

              { ...t, tranches: { ...t.tranches, deposits: newDeposits } }
          }
        }
    }

    pure def trackTrancheWithdrawal(t: TrackedValue, addr: Addr, key: TrancheKey, amount: int): TrackedValue = {
        pure val withdrawals = t.tranches.withdrawals
        pure val newWithdrawals =
            if (withdrawals.has((addr, key)))
                withdrawals.setBy((addr, key), v => v + amount)
            else
                withdrawals.put((addr, key), amount)

        // Reset swaps being tracked by for this address and tranche key
        pure val newSwaps = t.tranches.swaps.put((addr, key), Set())

        { ...t, tranches: { ...t.tranches, withdrawals: newWithdrawals, swaps: newSwaps } }
    }

    pure def trackTrancheCancelation(t: TrackedValue, addr: Addr, key: TrancheKey, makerAmount: int, takerAmount: int): TrackedValue = {
        pure val cancelations = t.tranches.cancelations
        pure val newCancelations =
            if (cancelations.has((addr, key)))
                cancelations.setBy((addr, key), v => (v._1 + makerAmount, v._2 + takerAmount))
            else
                cancelations.put((addr, key), (makerAmount, takerAmount))

        { ...t, tranches: { ...t.tranches, cancelations: newCancelations } }
    }

    pure def trackPoolDeposit(t: TrackedValue, addr: Addr, poolId: PoolId, amounts: (int, int), swappedAmounts: Option[(int, int)]): TrackedValue = {
        pure val deposits = t.pools.deposits
        pure val newDeposits =
            if (deposits.has((addr, poolId)))
                deposits.setBy((addr, poolId), (oldAmounts) => (oldAmounts._1 + amounts._1, oldAmounts._2 + amounts._2))
            else
                deposits.put((addr, poolId), amounts)

        pure val newSwaps = match swappedAmounts {
          // FIXME: this should actually update for other addresses but addr
          | Some(amounts) => t.pools.swaps.setByWithDefault((addr, poolId), s => s.union(Set({ token0Diff: amounts._1, token1Diff: amounts._2 })), Set())
          | None => t.pools.swaps
        }

        { ...t, pools: { ...t.pools, deposits: newDeposits, swaps: newSwaps } }
    }

    pure def trackPoolWithdrawal(t: TrackedValue, addr: Addr, poolId: PoolId, amounts: (int, int)): TrackedValue = {
        pure val withdrawals = t.pools.withdrawals
        pure val newWithdrawals =
            if (withdrawals.has((addr, poolId)))
                withdrawals.setBy((addr, poolId), (oldAmounts) => (oldAmounts._1 + amounts._1, oldAmounts._2 + amounts._2))
            else
                withdrawals.put((addr, poolId), amounts)

        // Reset swaps being tracked by for this address and pool id
        // FIXME: for pools, there can be partial withdrawals
        pure val newSwaps = t.pools.swaps.put((addr, poolId), Set())

        { ...t, pools: { ...t.pools, withdrawals: newWithdrawals, swaps: newSwaps } }
    }

    pure def trackSwap(t: TrackedValue, tranchesShares: Shares, poolShares: Addr -> PoolId -> int, swapUpdate: SwapUpdateInfo): TrackedValue = {
        pure val newTrancheSwaps = swapUpdate.tranchesUpdates.keys().fold(t.tranches.swaps, (swaps, key) => {
            pure val diffForKey = swapUpdate.tranchesUpdates.get(key)
            tranchesShares.keys().fold(swaps, (swaps, addr) => {
                if (tranchesShares.get(addr).keys().contains(key)) {
                     if (swaps.has((addr, key))) {
                         swaps.setBy((addr, key), s => s.union(Set(diffForKey)))
                     } else {
                         swaps.put((addr, key), Set(diffForKey))
                     }
                } else {
                    swaps
                }
            })
        })

        pure val newPoolSwaps = swapUpdate.poolUpdates.keys().fold(t.pools.swaps, (swaps, poolId) => {
            pure val diffForPool = swapUpdate.poolUpdates.get(poolId)
            poolShares.keys().fold(swaps, (swaps, addr) => {
                 if (poolShares.get(addr).keys().contains(poolId)) {
                     if (swaps.has((addr, poolId))) {
                         swaps.setBy((addr, poolId), s => s.union(Set(diffForPool)))
                     } else {
                         swaps.put((addr, poolId), Set(diffForPool))
                     }
                } else {
                    swaps
                }
            })
        })

        { tranches: { ...t.tranches, swaps: newTrancheSwaps }, pools: { ...t.pools, swaps: newPoolSwaps } }
    }
}
