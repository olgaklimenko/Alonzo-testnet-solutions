### 8. Define a Plutus minting script that allows you to mint a single instance of a non-fungible token. Your script should take a payment from a user-supplied address and pass this payment to an address of your choice.


#### Resources

[Plutus Pioneer Program lecture](https://www.youtube.com/watch?v=SsaVjSsPPcg)

#### Solution

NFT - is unique token (only one coin) that we can mint only once. So we need to limit the amount of tokens inside the minting policy.
If we just limit the mint value in TxInfo to 1, it wouldn't help. Because it will limit amount only within one transaction, so it would be possible to mint more than 1 coins using multiple transactions.
We should create validator in the way it will return True only once. So we need to use something unique to validate the NFT minting. Each UTxO is always unique. We can name a specific UTxO as parameter to the minting policy. And in policy we can check that transaction consumes that specific UTxO. 

To try minting policy for NFT, go to `plutus-scripts/policies/MintingNFTScript.hs` and edit `NFTMintingParameters` using UTxO, which you will use in `tx-in` of minting transaction and your token name.

Then run `nft-minting-policy` using this [instruction](https://github.com/olgaklimenko/Alonzo-testnet-solutions/blob/main/build-plutus-script.md).
Copy the resulting `nft-minting-policy.plutus` to data volume.

Create a policy Id:

```
cardano-cli transaction policyid --script-file nft-minting-policy.plutus > nft-minting-policy.id
export NFT_POLICY_ID=$(cat nft-minting-policy.id)
```

Make a transaction:

**WARNING:** My solution fails on transaction build. Working on fix.

```
cardano-cli transaction build \
--alonzo-era \
--testnet-magic 8 \
--tx-in 96667140a738478f579a640a90c76a18d7c4d9d0d3cadb2e7860f82e9ddda584#0 \
--tx-out $PERCY_ADDR+10000000+"1 $NFT_POLICY_ID.OlgaNFT" \
--change-address $PERCY_ADDR \
--mint="1 $NFT_POLICY_ID.OlgaNFT" \
--mint-redeemer-value '{"tokenName":"OlgaNFT","utxoId":"96667140a738478f579a640a90c76a18d7c4d9d0d3cadb2e7860f82e9ddda584","utxoIndex":0}' \
--mint-script-file nft-minting-policy.plutus \
--tx-in-collateral 18052dceb8d9d0da37191b5e9d0fba8705f16231f6b9400088f0a7aaffa49307#0 \
--protocol-params-file protocol.json \
--out-file mint-nft.raw

cardano-cli transaction sign \
         --signing-key-file /data/keys/percy.skey \
         --testnet-magic 8 \
         --tx-body-file mint-nft.raw \
         --out-file mint-nft.signed

cardano-cli transaction submit --tx-file  mint-nft.signed --testnet-magic 8
```
