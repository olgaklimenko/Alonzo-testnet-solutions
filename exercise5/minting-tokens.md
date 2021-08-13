
# Exercise 4

### Resources:

https://docs.cardano.org/native-tokens/learn
https://docs.cardano.org/native-tokens/getting-started
https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/multi-assets.md


### 1. Create a set of private/public signing keys, shelley, and two payment addresses, mary and percy. Fund the addresses with some test Ada

Creating `shelley` signing keys:

```
cardano-cli address key-gen \
    --verification-key-file shelley.vkey \
    --signing-key-file shelley.skey
```

Creating `mary` keys and address:

```
cardano-cli address key-gen \
--verification-key-file mary.vkey \
--signing-key-file mary.skey

cardano-cli address build \
    --payment-verification-key-file mary.vkey \
    --out-file mary.addr \
    --testnet-magic 8

export MARY_ADDR=$(cat mary.addr)

curl -v -k -XPOST "https://faucet.alonzo-purple.dev.cardano.org/send-money/$MARY_ADDR?apiKey=<ask key in Discord channel>"

# check Mary's balance
cardano-cli query utxo --address $MARY_ADDR --testnet-magic 8
```

The same actions are for `percy`:

```
cardano-cli address key-gen \
--verification-key-file percy.vkey \
--signing-key-file percy.skey

cardano-cli address build \
    --payment-verification-key-file percy.vkey \
    --out-file percy.addr \
    --testnet-magic 8

export PERCY_ADDR=$(cat percy.addr)

curl -v -k -XPOST "https://faucet.alonzo-purple.dev.cardano.org/send-money/$PERCY_ADDR?apiKey=<ask key in Discord channel>"

# check Percy's balance
cardano-cli query utxo --address $PERCY_ADDR --testnet-magic 8
```

### 2.1. Define a Mary-era minting script (a "multi-signature" script) that allows shelley to create new Ozymandian tokens. Define a minting policy for the Shelley currency that uses this minting script. Do not use Plutus scripts at this stage

(How to define a multi-signature script)[https://github.com/input-output-hk/cardano-node/blob/c6b574229f76627a058a7e559599d2fc3f40575d/doc/reference/simple-scripts.md]

Firstly the protocol parameters will be used in transaction, lets' request them.

```
cardano-cli query protocol-parameters \
  --testnet-magic 8 \
  --out-file protocol.json
```

Create a hash for `shelley` vkey and use it in policy script:

```
echo "{" > policy.script 
echo "  \"keyHash\": \"$(cardano-cli address key-hash --payment-verification-key-file /data/keys/shelley.vkey
)\"," >> policy.script 
echo "  \"type\": \"sig\"" >> policy.script 
echo "}" >> policy.script 
```

Mint the new asset with our policy:

```
export SHELLEY_POLICY_ID=$(cardano-cli transaction policyid --script-file policy.script)
```

### Mint 1000 new Ozymandians in the percy address by building and submitting a transaction. Check that they have been successfully minted.

Build the draft transaction to calculate a fee:
Tx-in is a utxo on Percy address, tx-out is Percy address + 1M Ada we got from faucet + 1000 new Ozymandians

```
cardano-cli transaction build-raw \
    --alonzo-era \
    --tx-in 5527cc166d524c0ed50109073fb9c496957a8607f8994c889ed2f06fdd954c31#0 \
    --tx-out $PERCY_ADDR+1000000000000+"1000000000 $SHELLEY_POLICY_ID.Ozymandian" \
    --mint="1000000000 $SHELLEY_POLICY_ID.Ozymandian" \
    --mint-script-file policy.script \
    --fee 0 \
    --out-file mint_ozymandians.draft
```

Calculate the minimal fee for the minting transaction:

```
cardano-cli transaction calculate-min-fee \
    --tx-body-file mint_ozymandians.draft \
    --tx-in-count 1 \
    --tx-out-count 1 \
    --witness-count 2 \
    --testnet-magic 8 \
    --protocol-params-file protocol.json

182221 Lovelace
```

Calculate the remaining ADA after paying fee: 1000000000000 - 182221 = 999999817779
Make a raw transaction: 

```
cardano-cli transaction build-raw \
    --alonzo-era \
    --tx-in 5527cc166d524c0ed50109073fb9c496957a8607f8994c889ed2f06fdd954c31#0 \
    --tx-out $PERCY_ADDR+999999817779+"1000000000 $SHELLEY_POLICY_ID.Ozymandian" \
    --mint="1000000000 $SHELLEY_POLICY_ID.Ozymandian" \
    --mint-script-file policy.script \
    --fee 182221 \
    --out-file mint_ozymandians.raw
```

Sign and submit the transaction:

```
cardano-cli transaction sign \
         --signing-key-file /data/keys/percy.skey \
         --signing-key-file /data/keys/shelley.skey \
         --testnet-magic 8 \
         --tx-body-file mint_ozymandians.raw \
         --out-file mint_ozymandians.signed

cardano-cli transaction submit --tx-file  mint_ozymandians.signed --testnet-magic 8
```
Wait a minute, and it's time to check Percy's wallet!

`cardano-cli query utxo --address $PERCY_ADDR --testnet-magic 8`

Percy have Ozymandians now, don't you?

### 4. Define a second minting script that allows shelley to create new SkyLark tokens. Mint 100 SkyLark tokens and send them to percy. Check that the tokens have been received and then send 75 SkyLark tokens to mary.

Don't know why we need to define a new policy, I will use the previous one to forge some SkyLarks for Percy.

```
cardano-cli transaction build-raw \
    --alonzo-era \
    --tx-in b87bfdbd6ed7f709fa524e1d08e327af4db353377778aa7029dfae691d4e88da#0 \
    --tx-out $PERCY_ADDR+999999635558+"1000000000 $SHELLEY_POLICY_ID.Ozymandian"+"100000000 $SHELLEY_POLICY_ID.SkyLark" \
    --mint="100000000 $SHELLEY_POLICY_ID.SkyLark" \
    --mint-script-file policy.script \
    --fee 182221 \
    --out-file mint_skylark.raw
```

Sign and submit the transaction:

```
cardano-cli transaction sign \
         --signing-key-file /data/keys/percy.skey \
         --signing-key-file /data/keys/shelley.skey \
         --testnet-magic 8 \
         --tx-body-file mint_skylark.raw \
         --out-file mint_skylark.signed

cardano-cli transaction submit --tx-file  mint_skylark.signed --testnet-magic 8
```

Send 75 SkyLark tokens to Mary. Every transaction output must contain some ada. This is because there is a minimum value of ada that is needed per transaction output. The minimum value of transaction output is 1ADA.

```
cardano-cli transaction build-raw \
--tx-in 3611c07ed9a8c44eb671f133e2fba3efbc9df1c86f9a992c791b3357c909021f#0 \
--tx-out $MARY_ADDR+5000000+"75000000 $SHELLEY_POLICY_ID.SkyLark" \
--tx-out $PERCY_ADDR+0 \
--fee 0 \
--out-file skylarks_to_mary.draft

cardano-cli transaction calculate-min-fee \
    --tx-body-file skylarks_to_mary.draft \
    --tx-in-count 1 \
    --tx-out-count 2 \
    --witness-count 1 \
    --testnet-magic 8 \
    --protocol-params-file protocol.json

> 176017 Lovelace

expr 999999635558 - 5000000 - 176017 
>999998459541

cardano-cli transaction build-raw \
--tx-in 3611c07ed9a8c44eb671f133e2fba3efbc9df1c86f9a992c791b3357c909021f#0 \
--tx-out $MARY_ADDR+5000000+"75000000 $SHELLEY_POLICY_ID.SkyLark" \
--tx-out $PERCY_ADDR+999994459541+"1000000000 $SHELLEY_POLICY_ID.Ozymandian"+"25000000 $SHELLEY_POLICY_ID.SkyLark" \
--fee 176017 \
--out-file skylarks_to_mary.raw

cardano-cli transaction sign \
         --signing-key-file /data/keys/percy.skey \
         --testnet-magic 8 \
         --tx-body-file skylarks_to_mary.raw \
         --out-file skylarks_to_mary.signed

cardano-cli transaction submit --tx-file  skylarks_to_mary.signed --testnet-magic 8
```

### 5. What is the least amount of Ada that you need to keep in the mary and percy addresses? What is the least amount of Ozymandians or SkyLarks that you can keep in an address?

For any transaction 1 ADA + tx fees is required, for other tokens we haven't any minimal limit.


### 6. You want to burn some of your Ozymandians in the percy address_._ How do you do this? What happens to your Ada balances when you burn your tokens?

Tokens can be destroyed by a token issuer according to the token policy by supplying a negative value in the --mint field. Let's burn 150 Ozymandians.

```
Fee will be the same as for minting, as we have the same number of tx-in and tx-out, and 2 witnesses (shelley and percy).

expr 999994459541 - 182221 = 999994277320
expr 1000000000 - 150000000 = 850000000
cardano-cli transaction build-raw \
    --alonzo-era \
    --tx-in 9bd45e383cc2276acdda17849586ca2429724b18417483559508343cac222242#1 \
    --tx-out $PERCY_ADDR+999994277320+"850000000 $SHELLEY_POLICY_ID.Ozymandian"+"25000000 $SHELLEY_POLICY_ID.SkyLark" \
    --mint="-150000000 $SHELLEY_POLICY_ID.Ozymandian" \
    --mint-script-file policy.script \
    --fee 182221 \
    --out-file burn_ozymandians.raw

cardano-cli transaction sign \
         --signing-key-file /data/keys/percy.skey \
         --signing-key-file /data/keys/shelley.skey \
         --testnet-magic 8 \
         --tx-body-file burn_ozymandians.raw \
         --out-file burn_ozymandians.signed

cardano-cli transaction submit --tx-file  burn_ozymandians.signed --testnet-magic 8
```

### 7. Define a Plutus minting script that allows you to mint a variable number of Ozymandian and SkyLark tokens (with the numbers supplied via a redeemer). Verify that this works as you expect.

https://reddspark.blog/2021/08/02/simply-plutus-minting-policies/

In minting policy we can validate the `TxInfoForge` the Value minting by this transaction. We can get it from the `Script context`'s `TxInfo` data and check if the number from redeemer equals the number in `TxInfoForge`.

Go to the `policies` directory and run `plutus-minting-purple-example` using this [instruction](../build-plutus-script). The log out put will be:

```
ExBudget {_exBudgetCPU = ExCPU 19486000, _exBudgetMemory = ExMemory 4740}
"Currency symbol: 30365daa7b21d46f4930876ee0eba865650533e7cb92ad662f7d68f4b44086f9"
```

Currency symbol is the same as a policyId in terms of plutus scripts. You can receive the currency symbol using `scriptCurrencySymbol` and print it to the application's log.

Copy the resulting `minting-policy-purple.plutus` to data volume, and make a script address:

```
cardano-cli address build --payment-script-file minting-policy-purple.plutus --testnet-magic 8 --out-file check_number.addr

export CHECK_NUMBER_ADDR=$(cat check_number.addr)
```

export minting policy id (currency symbol):
```
export CHECK_NUMBER_POLICY=30365daa7b21d46f4930876ee0eba865650533e7cb92ad662f7d68f4b44086f9
```

Make a collateral utxo for percy:

```
cardano-cli transaction build-raw \
--tx-in 0f3a00cb06b7f883a85d8f0b9b4cd37c88075d76ba32e7ed6de3429d2620c2f2#1 \
--tx-out $PERCY_ADDR+10000000000 \
--tx-out $ADDRESS+929999467153 \
--fee 176545 \
--out-file make-collateral.raw
```

**WARNING** next things are not completed, and aren't working now

<!-- ```
cardano-cli transaction build-raw \
--alonzo-era \
--tx-in 6f1aea867857746d412ea7010ba427d5ba05a496194ca8fb27186ecbbe731f96#0 \
--tx-out $PERCY_ADDR+0 \
--fee 1001000000 \
--mint="25000000 $CHECK_NUMBER_POLICY.SkyLark" \
--mint-redeemer-value '{"mpTokenName":"SkyLark","mpAmount":25000000}' \
--mint-execution-units "(20422000, 100000)" \
--mint-script-file minting-policy-purple.plutus \
--tx-in-collateral e5d0c6d1aad24bd26bac9752a9f4cd22d68b75b7bd078601bd99d3abf49c48cf#0 \
--protocol-params-file protocol.json \
--out-file check-number.raw

``` -->

```
cardano-cli transaction build \
--alonzo-era \
--testnet-magic 8 \
--tx-in 6f1aea867857746d412ea7010ba427d5ba05a496194ca8fb27186ecbbe731f96#0 \
--change-address $PERCY_ADDR \
--mint="25000000 $CHECK_NUMBER_POLICY.SkyLark" \
--mint-redeemer-value '{"mpTokenName":"SkyLark","mpAmount":25000000}' \
--mint-script-file minting-policy-purple.plutus \
--tx-in-collateral e5d0c6d1aad24bd26bac9752a9f4cd22d68b75b7bd078601bd99d3abf49c48cf#0 \
--protocol-params-file protocol.json \
--out-file check-number.raw


```
cardano-cli transaction sign \
         --signing-key-file /data/keys/percy.skey \
         --testnet-magic 8 \
         --tx-body-file check-number.raw \
         --out-file check-number.signed

cardano-cli transaction submit --tx-file  check-number.signed --testnet-magic 8

```

