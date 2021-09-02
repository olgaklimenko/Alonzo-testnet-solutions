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

Other steps are the same as for the [Plevious minting script](https://github.com/olgaklimenko/Alonzo-testnet-solutions/blob/main/exercise5/minting-tokens-plutus.md), just `redeemer` data needs to be changed.
