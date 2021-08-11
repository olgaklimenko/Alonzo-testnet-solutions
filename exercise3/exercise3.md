# excercise3

How to make a transaction resources:

`cardano-cli transaction build-raw --help`

[transaction tutorial](https://github.com/input-output-hk/cardano-node/blob/master/doc/stake-pool-operations/simple_transaction.md)

Get protocol configuration for Alonzo purple:

```
cardano-cli query protocol-parameters \
  --testnet-magic 8 \
  --out-file protocol.json
```

```
cardano-cli transaction build-raw \
--tx-in 9a5c946abe22920e2588c30340cc4c882596f820acd06fc9245ffef8c70b97ab#0 \
--tx-out $ADDRESS2+0 \
--tx-out $ADDRESS1+0 \
--invalid-hereafter 0 \
--fee 0 \
--out-file tx.draft

cardano-cli transaction calculate-min-fee \
--tx-body-file tx.draft \
--tx-in-count 1 \
--tx-out-count 2 \
--witness-count 1 \
--byron-witness-count 0 \
--testnet-magic 8 \
--protocol-params-file protocol.json

expr 943998760973 - 10000000000 - 176545 = 933998584428

cardano-cli query tip --testnet-magic 8

expr 1503493 + 200 = 1503693

cardano-cli transaction build-raw \
--tx-in 2c609a24c7d328fe945c8dc8edf0a0c1898f5bdbb75695fe73885c007bc8ede3#1 \
--tx-out $ADDRESS2+10000000000 \
--tx-out $ADDRESS1+933998584428 \
--fee 176545 \
--out-file tx.raw

cardano-cli transaction sign \
--tx-body-file tx.raw \
--signing-key-file /data/keys/wallet1/payment.skey \
--testnet-magic 8 \
--out-file tx.signed
&
cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 8 
```

# Part2: Lock funds.

```
echo 99 > random_datum.txt

cardano-cli transaction hash-script-data --script-data-value $(cat random_datum.txt)

echo "{
    \"type\": \"PlutusScriptV1\",
    \"description\": \"\",
    \"cborHex\": \"585c585a010000332233322233333322222233222220051200120012122222300500622122222330040070062122222300300621222223002006212222230010062001112200212212233001004003120011122123300100300211200101\"
}
" > AlwaysSucceeds.plutus

cardano-cli address build --payment-script-file AlwaysSucceeds.plutus --testnet-magic 8 --out-file script.addr

cardano-cli transaction build-raw \
--alonzo-era \
--tx-in 4a182fc2929949f8b480ab556639d263f55d7b0969e4f1fe5013baf775c5d495#1 \   # tx on ADDRESS2
--tx-out $SCRIPT_ADDRESS+0 \
--tx-out-datum-hash e2f7674de02201c3cf68561107036c021e5c808199e810f8324efc71a6738002 \
--tx-out $ADDRESS2+0 \
--fee 0 \
--protocol-params-file ../../protocol.json \
--out-file tx.draft

cardano-cli transaction calculate-min-fee \
--tx-body-file tx.draft \
--tx-in-count 1 \
--tx-out-count 3 \
--witness-count 1 \
--byron-witness-count 0 \
--testnet-magic 8 \
--protocol-params-file ../../protocol.json  #179757 Lovelace

expr 1000000000000 - 5000000000 - 179757
994999820243

cardano-cli transaction build-raw \
--alonzo-era \
--tx-in 4a182fc2929949f8b480ab556639d263f55d7b0969e4f1fe5013baf775c5d495#1 \
--tx-out addr_test1wz9q37z3kgh9c4x7ppa7xplw4va4epvg4r82svvcvlrcdcqzuzfp7+500000000 \
--tx-out-datum-hash e2f7674de02201c3cf68561107036c021e5c808199e810f8324efc71a6738002 \
--tx-out $ADDRESS2+994499640486 \
--fee 179757 \
--protocol-params-file ../../protocol.json \
--out-file tx.raw

cardano-cli transaction sign \
--tx-body-file tx.raw \
--signing-key-file /data/keys/wallet2/payment.skey \
--testnet-magic 8 \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 8 
```

# Part 3: Unlocking funds that are guarded by a Plutus script.

```
Strictly speaking, collateral is paid if unsuccessful, and fee if successful;
https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/simple-scripts.md
https://github.com/input-outputhk/cardano-node/blob/master/doc/reference/plutus/plutus-spending-script-example.md

txFee <- H.noteShow $ plutusRequiredTime + plutusRequiredSpace
txinCollateral <- H.noteShow $ head $ HM.keys utxo2

cardano-cli transaction build-raw \
--alonzo-era \
--fee 0 \
--tx-in 4a182fc2929949f8b480ab556639d263f55d7b0969e4f1fe5013baf775c5d495#0 \ # tx on SCRIPT_ADDRESS with corresponding DATUM 
--tx-in-script-file ../AlwaysSucceeds.plutus \
--tx-in-datum-value $(cat ../random_datum.txt) \
--tx-in-redeemer-value $(cat ../random_datum.txt) \
--tx-in-execution-units "(0,0)" \
--tx-in-collateral 3db1b0afc352a8f70b1cb0c7ba4f7d5eba0b0319af204ca7300050c709697fe3#1 \ # tx on ADDRESS2
--tx-out $ADDRESS2+0 \
--protocol-params-file ../../protocol.json \
--out-file tx.draft

cardano-cli transaction calculate-min-fee \
--tx-body-file tx.draft \
--tx-in-count 6 \
--tx-out-count 1 \
--witness-count 1 \
--byron-witness-count 0 \
--testnet-magic 8 \
--protocol-params-file ../../protocol.json  # 187941

expr 5000000000 - 200187941 = 299812059 # script amount - fee
100000000 + 100000000 + 187941 = 200187941

cardano-cli transaction build-raw \
--fee 200187941 \
--alonzo-era \
--tx-in a1c427ec3d9a3202d1401b61fe585bbbaafc3576542c56004a0d54c38ff37b68#0 \
--tx-in-script-file ../AlwaysSucceeds.plutus \
--tx-in-datum-value $(cat ../random_datum.txt) \
--tx-in-redeemer-value $(cat ../random_datum.txt) \
--tx-in-execution-units "(100000000,100000000)" \
--tx-in-collateral 8ffc217d70e982972d6cb8f4eca196c355d5f058b76e958987012d485b556d14#0 \
--tx-out $ADDRESS2+4799812059 \
--protocol-params-file ../../protocol.json \
--out-file tx.raw

cardano-cli transaction sign \
--tx-body-file tx.raw \
--signing-key-file /data/keys/wallet2/payment.skey \
--testnet-magic 8 \
--out-file tx.signed

cardano-cli transaction submit \
--tx-file tx.signed \
--testnet-magic 8 
```