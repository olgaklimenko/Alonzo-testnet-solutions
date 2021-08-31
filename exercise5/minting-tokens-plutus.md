### 7. Define a Plutus minting script that allows you to mint a variable number of Ozymandian and SkyLark tokens (with the numbers supplied via a redeemer). Verify that this works as you expect.

#### Resources

[How to create plutus minting policy script](https://reddspark.blog/2021/08/02/simply-plutus-minting-policies/)

[Minting script example](https://github.com/input-output-hk/Alonzo-testnet/blob/main/resources/plutus-sources/plutus-example/src/Cardano/PlutusExample/MintingScriptPurple.hs)

#### Solution

In minting policy we can validate the `TxInfoMint` the Value minting by this transaction. We can get it from the `Script context`'s `TxInfo` data and check if the number from redeemer equals the number in `txInfoMint`.

Go to the `plutus-scripts` directory and run `plutus-minting-purple-example` using this [instruction](https://github.com/olgaklimenko/Alonzo-testnet-solutions/blob/main/build-plutus-script.md).
Copy the resulting `minting-policy-purple.plutus` to data volume, and make a script address:

Export a minting policy id:

```
cardano-cli transaction policyid --script-file minting-policy-purple.plutus > minting-policy-purple.id
# export CHECK_AMOUNT_POLICY=30365daa7b21d46f4930876ee0eba865650533e7cb92ad662f7d68f4b44086f9
export CHECK_AMOUNT_POLICY_ID=$(cat minting-policy-purple.id)
```

Make a collateral utxo for Percy:

```
cardano-cli transaction build-raw \
--tx-in 937529af1cc6064de69019af82b2d4ba35052b712f88d84d55c5bf7f6174b778#1 \
--tx-out $PERCY_ADDR+100000000000 \
--tx-out $PERCY_ADDR+889999646910 \
--fee 176545 \
--out-file make-collateral.raw

cardano-cli transaction sign \
         --signing-key-file /data/keys/percy.skey \
         --testnet-magic 8 \
         --tx-body-file make-collateral.raw \
         --out-file make-collateral.signed

cardano-cli transaction submit --tx-file  make-collateral.signed --testnet-magic 8
```

Now all is ready for minting:

```
cardano-cli transaction build \
--alonzo-era \
--testnet-magic 8 \
--tx-in 24f38a5f0296efc28210bd9503e0fae97edf0f3abaa2c3304d7d4b01336a3707#0 \
--tx-out $PERCY_ADDR+10000000+"150 $CHECK_AMOUNT_POLICY_ID.SkyLark" \
--change-address $PERCY_ADDR \
--mint="150 $CHECK_AMOUNT_POLICY_ID.SkyLark" \
--mint-redeemer-value 150 \
--mint-script-file minting-policy-purple.plutus \
--tx-in-collateral 18052dceb8d9d0da37191b5e9d0fba8705f16231f6b9400088f0a7aaffa49307#0 \
--protocol-params-file protocol.json \
--out-file check-amount.raw

cardano-cli transaction sign \
         --signing-key-file /data/keys/percy.skey \
         --testnet-magic 8 \
         --tx-body-file check-amount.raw \
         --out-file check-amount.signed

cardano-cli transaction submit --tx-file  check-amount.signed --testnet-magic 8
```
