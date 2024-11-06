// -*- mode: Bluespec; -*-
module invariants{
    import dec.* from "lib/dec"
    import types.* from "types"
    import consts.* from "consts"
    import utils.* from "utils"
    import basicSpells.* from "lib/basicSpells"
    import Dex.* from "dex"
    import dexEvolution.* from "dexEvolution"
    import postconditions.* from "postconditions"

    // Postconditions always hold
    val postconditions = postconditionOK

    // Pool reserves are always greater or equal to zero 
    val nonNegativeReserves = state.pools.keys().forall(
        poolId => 
            val reserves = state.pools.get(poolId).reserves
            reserves._1 >= 0 and reserves._2 >= 0
        )

    // Invariant: shares supplies are always greater or equal to zero 
    val nonNegativeSharesSupply = state.pools.keys()
        .forall(poolId => state.pools.get(poolId).shares >= 0)

    // Invariant: coin and share balances are zero or greater 
    val nonNegativeBalances = CREATORS.forall(creator => 
        val coins = state.coins.getOrElse(creator, Map())
        val shares = state.poolShares.getOrElse(creator, Map())
        coins.keys().forall(token => coins.get(token) >= 0) and
        shares.keys().forall(share => shares.get(share) >= 0)
        )
    
    // Invariant: LP profit from their deposits
    // simplified LP.5 in specification invariants
    val profitDeposit = CREATORS.forall(
        creator => state.deposits.get(creator).keys().forall(poolId => 
            // If the user has withdrawn all their shares
            if (state.poolShares.get(creator).get(poolId) == 0)
                val deposited = state.deposits.get(creator).getOrElse(poolId, {amounts: (0,0), swaps: false})
                val withdrawnAmounts = state.poolWithdraws.get(creator).getOrElse(poolId, (0,0))                                
                // If there were swaps, the user withdrew more value than deposited
                if (deposited.swaps) approx_gt(                    
                    computeDepositValue(withdrawnAmounts, poolId.tick),
                    computeDepositValue(deposited.amounts, poolId.tick),
                    fromInt(TOLERANCE)
                )                  
                // Otherwise, the user withdrew no less value than deposited  
                else approxEqualDec(
                    computeDepositValue(withdrawnAmounts, poolId.tick), 
                    computeDepositValue(deposited.amounts, poolId.tick), 
                    fromInt(TOLERANCE)
                    )
            // This is the case in which the user did not withdraw all the shares so we are not claiming anything here
            else true
            )            
        )

    // Invariant: Tranche expiration times always >= -1 (sanity check)
    val expirationTimeSanity = 
        state.tranches.keys().forall(k => state.tranches.get(k).expirationTime >= -1)


    // totalMaker for a tranche equals the sum of all sharesOwned by the users
    val totalMakerInvariant = state.tranches.keys().forall(
       trancheKey =>
            val totalMaker = state.tranches.get(trancheKey).totalMaker
            val sharesOwnedSum = state.tranchesShares.keys().fold(
                0,
                (acc, creator) => 
                val sharesOwnedByUser = 
                    if (state.tranchesShares.get(creator).keys().contains(trancheKey)) 
                        state.tranchesShares.get(creator).get(trancheKey).sharesOwned
                    else 0
                acc + sharesOwnedByUser
            )
            require(totalMaker == sharesOwnedSum)
        )

    // A user's placed orders will retain their value: what a user placed in a tranche will match the sum of 
    // existing reserves (both taker and maker) and withdrawn coins (both taker and maker)
    // 
    // TODO: in order to implement this invariant, I will need another bookkeeping variable to keep track of the
    // coins withdrawn from particular tranches (similar to what deposits and poolWithdraws are doing for pools)
    val makerValuePreserved = true


}