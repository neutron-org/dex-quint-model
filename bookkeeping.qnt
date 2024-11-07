module bookkeeping {
    import basicSpells.* from "lib/basicSpells"
    import types.* from "types"

    pure val emptyTrackedValue: TrackedValue = {
        tranches: {
            deposits: Map(),
            withdrawals: Map(),
            cancelations: Map(),
        }
    }

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

        { ...t, tranches: { ...t.tranches, withdrawals: newWithdrawals } }
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
}
