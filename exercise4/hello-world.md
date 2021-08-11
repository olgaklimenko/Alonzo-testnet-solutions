# Hello World script

## Preparations for the task

- Create an address and query test ada from the faucet, as described in exercise 1, and export address as $ADDRESS.

- Find the `plutus-helloworld-bytestring` app in `Alonzo-testnet` repo, and build it ([Read here](../build-plutus-script) how to build). In the log output of `cabal run` will be Exbudget structure:

```
ExBudget {_exBudgetCPU = ExCPU 24166000, _exBudgetMemory = ExMemory 5940}
```
these units will be helpful in `--tx-in-execution-units` option, when we will try to run the execution of the script on chain. 
 
**Note:** for the testnet we need to multiply these units by some coefficient, what exactly you can ask in discord channels.

After run the executable you will have a `*.plutus` file in you project directory, let's rename it to `hello-world-bs.plutus` and copy it to `data` directory to make it available in the docker container.

## 1 Lock

```
mkdir hello-world & cd hello-world

cardano-cli address build --payment-script-file hello-world-bs.plutus --testnet-magic 8 --out-file hello-world-bs.addr

export HELLO_WORLD_ADDR=$(cat hello-world-bs.addr)

cardano-cli transaction hash-script-data --script-data-value '"Hello World!"' > hello_world_hash.txt
# 9ad30ffde0d1931ed4f145fa0a0d320a067051bfab1b08cbdb79e9f26df55df3

export HELLO_WORLD_HASH=$(cat hello_world_hash.txt)

cardano-cli transaction build-raw \
--alonzo-era \
--tx-in 30fe40b298b2f2d2683bd248ca00d123f02894581fce66a3b5289533bd30ffa7#0 \
--tx-out $HELLO_WORLD_ADDR+0 \
--tx-out-datum-hash $HELLO_WORLD_HASH \
--tx-out $ADDRESS+0 \
--fee 0 \
--protocol-params-file protocol.json \
--out-file hello-world-lock.draft

cardano-cli transaction calculate-min-fee \
--tx-body-file hello-world-lock.draft \
--tx-in-count 1 \
--tx-out-count 3 \
--witness-count 1 \
--byron-witness-count 0 \
--testnet-magic 8 \
--protocol-params-file protocol.json  #179757 Lovelace

expr 1000000000000 - 50000000000 - 179757
> 949999820243

cardano-cli transaction build-raw \
--alonzo-era \
--tx-in 30fe40b298b2f2d2683bd248ca00d123f02894581fce66a3b5289533bd30ffa7#0 \
--tx-out $HELLO_WORLD_ADDR+50000000000 \
--tx-out-datum-hash $HELLO_WORLD_HASH \
--tx-out $ADDRESS+949999820243 \
--fee 179757 \
--protocol-params-file protocol.json \
--out-file hello-world-lock.raw

cardano-cli transaction sign \
--tx-body-file hello-world-lock.raw \
--signing-key-file /data/keys/address.skey \
--testnet-magic 8 \
--out-file hello-world-lock.signed

cardano-cli transaction submit \
--tx-file hello-world-lock.signed \
--testnet-magic 8 
```

Check if new utxo is on the script address:

```
cardano-cli query utxo --address $HELLO_WORLD_ADDR --testnet-magic 8
```

# Make a utxo for the collateral (It will be spent if validation is failed)

```
cardano-cli transaction build-raw \
--tx-in ddb80654d2a018d4e6a0eb64b751038268de19ad81ab3844129400560f8a6781#1 \
--tx-out $ADDRESS+10000000000 \
--tx-out $ADDRESS+939999643698 \
--fee 176545 \
--out-file make-collateral.raw

expr 949999820243 - 10000000000 - 176545 
>939999643698

cardano-cli transaction sign \
--tx-body-file make-collateral.raw \
--signing-key-file /data/keys/address.skey \
--testnet-magic 8 \
--out-file make-collateral.signed
&
cardano-cli transaction submit \
--tx-file make-collateral.signed \
--testnet-magic 8 
```

# 1 Unlock

Tx-in is a utxo on hello-world script address. Redeemer can be any Bytestring here, datum should be the same as we used to get datum-hash for the lock transaction.

```
expr 50000000000 - 1001000000 = 49199000000

cardano-cli transaction build-raw \
--alonzo-era \
--fee 1001000000 \
--tx-in 1dbf6145cd287eb8ee0f49334a0a9e20bf7ab0da792f71d2a40476cd1859219b#0 \
--tx-in-script-file hello-world-bs.plutus \
--tx-in-datum-value '"Hello World!"' \
--tx-in-redeemer-value '"Hello World!"' \
--tx-in-execution-units "(300000000,100000)" \
--tx-in-collateral 0f3a00cb06b7f883a85d8f0b9b4cd37c88075d76ba32e7ed6de3429d2620c2f2#0 \
--tx-out $ADDRESS2+48999000000 \
--protocol-params-file ../../../protocol.json \
--out-file tx.raw

Or you can try new `Purple` 

cardano-cli transaction build \
--alonzo-era \
--testnet-magic 8 \
--tx-in ddb80654d2a018d4e6a0eb64b751038268de19ad81ab3844129400560f8a6781#0 \
--tx-in-collateral 0f3a00cb06b7f883a85d8f0b9b4cd37c88075d76ba32e7ed6de3429d2620c2f2#0 \
--tx-in-script-file hello-world-bs.plutus \
--tx-in-datum-value '"Hello World!"' \
--tx-in-redeemer-value '"Hello World!"' \
--change-address $ADDRESS \
--protocol-params-file protocol.json \
--out-file hello-world-unlock.raw

cardano-cli transaction sign \
--tx-body-file hello-world-unlock.raw \
--signing-key-file /data/keys/address.skey \
--testnet-magic 8 \
--out-file hello-world-unlock.signed

cardano-cli transaction submit \
--tx-file hello-world-unlock.signed \
--testnet-magic 8 
```
