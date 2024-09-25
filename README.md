# Neutron DEX Model
This repository is a work-in-progress specification of the Neutron DEX module. 

- Ongoing modeling: limit orders + pools deposits and their interactions :hourglass:

 

## Repository Structure

 - `lib`: generally useful Quint functions, for modelling arithmetic operatoins
 - `history`: previous, related models (developed in 2023 during earlier audits), some of them unfinished
 -  The files that make the model in progress:
    - `dex.qnt`: the model
    - `utils.qnt`: helper functions
    - `consts.qnt`: constants used for simulation (eg, max amounts to trade with etc.)
    - `types.qnt`: basic types used in the model
    - `runs.qnt`: a sequence 

## How To Run Interact with the model

### Requirements

- [Quint](https://github.com/informalsystems/quint)

### Resources

- [Quint cheatsheet](https://github.com/informalsystems/quint/blob/main/doc/quint-cheatsheet.pdf)
- [Quint tutorials](https://github.com/informalsystems/quint/tree/main/tutorials)

### Interact with the model in a REPL


```shell
# Load the dex file
quint --r dex.qnt::Dex
```

This command will open Quint REPL. Inside the Quint REPL:

Initialize the state by executing the `init` action
```quint
init
```

Check the state by running `state`, or its fields, e.g., `state.tranches`.

Nothing very interesting for the moment.
Thus, let's run an action, e.g., `placeLimitOrderAction`.
This will create a non-deterministic `PlaceLimitOrderMsg` and change the `state`.

Alternatively, run `step`: this will pick any step to be executed. 
For inspecting the choices of nondeterministic values, run

If you want a to experiment with concrete messages, try something like this:

```quint
val bobsOrder: PlaceLimitOrderMsg = {
                creator: "Bob",
                orderType: GoodTillCancelled,
                tokenIn: "A",
                tokenOut: "B",
                amountIn: 10,
                tick: 19460,
                maxAmountOut: -1
            }
```
and then `state' = placeLimitOrder(state, bobsOrder).state`.

For diggingg deeper, a REPL may be run with the `--verbosity=5` flag: in that case, it will show an evaluation of each function of the model.


### Running sample runs

This is already getting verbose. 
In order to keep it out of the REPL, we use *runs* - one initial run is sketched in [runs.qnt](runs.qnt).

Use it inside the REPL and outside of it. 
You can start the REPL with `quint -r runs.qnt::runs`, and inside it call the run `limitOrderTrade`.
This can be useful when you want to start with a particular state, as reached by a run.

Alternatively, run `quint test --match limitOrderTrade --max-samples=1 runs.qnt`.
This command will run `limitOrderTrade` and check the assertions in it.



### Run the Simulator
The Quint simulator creates a number of behaviors allowed by the model. You can run the simulator by
`quint run --max-samples=1 --max-steps=20 --out-itf=out.itf.json dex.qnt`
After doing that, inspect the trace in the file `out.itf.json` ->  in order to to visualize trace, you should also install the [VSCode plugin for visualizing traces](https://marketplace.visualstudio.com/items?itemName=informal.itf-trace-viewer).

NOTE: there are still some runtime errors occurring that need to be ironed out.

## Still missing from the model
 - clearing of inactive orders each block
 - proper handling of numerics
 - incorporating different order types to postconditions
 - a richer set of invariants
 - `maxAmountOut` parameter for placing limit orders
 - `behindEnemyLines` check
 - currently, `MultiHopSwap` is abstracted away by a `SingleHopSwap`
    - `SingleHopSwap` needs to be unified with the `SingleHopSwap` postcondition

