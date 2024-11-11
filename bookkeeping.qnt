// -*- mode: Bluespec; -*-
module bookkeeping {
    import basicSpells.* from "lib/basicSpells"
    import types.* from "types"

    pure val emptyTrackedValue: TrackedValue = {
        tranches: {
            placements: Map(),
            withdrawals: Map(),
            cancelations: Map(),
            swaps: Map(),
        }
    }

    pure def trackTranchePlacement(t: TrackedValue, addr: Addr, key: Option[TrancheKey], amount: int): TrackedValue = {
        match key {
          | None => t
          | Some(key) => {
              pure val placements = t.tranches.placements
              pure val newPlacements =
                  if (placements.has((addr, key)))
                      placements.setBy((addr, key), v => v + amount)
                  else
                      placements.put((addr, key), amount)

              { ...t, tranches: { ...t.tranches, placements: newPlacements } }
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

    pure def trackSwap(t: TrackedValue, tranchesShares: Shares, swapUpdate: SwapUpdateInfo): TrackedValue = {
        // For now, we only need to track swaps on tranches, not on pools
        pure val newSwaps = swapUpdate.tranchesUpdates.keys().fold(t.tranches.swaps, (swaps, key) => {
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
        { ...t, tranches: { ...t.tranches, swaps: newSwaps } }
    }
}
