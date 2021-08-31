# Start alonzo-purple node using docker

Find an actual image for alonzo-purple in the dockerhub: https://hub.docker.com/r/inputoutput/cardano-node/tags?page=1&ordering=last_updated.

Get a config name for alonzo-purple here: https://hydra.iohk.io/build/7189190/download/1/index.html

```
ALONZO_CONFIG=alonzo-purple
ALONZO_IMAGE_TAG=1.29.0-rc2

docker run --rm -it \
    -e NETWORK=$ALONZO_CONFIG \
    -v /media/hdd/cardano/purple/node-data:/data/db \
    -v /media/hdd/cardano/purple/node-ipc:/ipc \
    -v ~/testnet/keys:/data/keys \
    -v ~/testnet/exercises-data:/data/excercises \
    -p 3001:3001 \
    -e CARDANO_NODE_SOCKET_PATH=/ipc/node.socket \
    --name purple \
    inputoutput/cardano-node:$ALONZO_IMAGE_TAG

docker exec -ti purple bash
```

# Create keys and address:

Resource: https://github.com/input-output-hk/cardano-node/blob/master/doc/stake-pool-operations/keys_and_addresses.md

Testnet-magic parameter is the `networkMagic` value in `shelley-genesis.json` config. For my case it is `8`: https://hydra.iohk.io/build/7189190/download/1/alonzo-purple-shelley-genesis.json

```
cardano-cli address key-gen \
--verification-key-file address.vkey \
--signing-key-file address.skey

cardano-cli address build \
    --payment-verification-key-file address.vkey \
    --out-file address.addr \
    --testnet-magic 8

export ADDRESS=$(cat address.addr)
```

# Add test ada to address

```
KEY=<ask key in Discord channel>
curl -v -k -XPOST "https://faucet.alonzo-purple.dev.cardano.org/send-money/$ADDRESS?apiKey=$KEY"

cardano-cli query utxo --address $ADDRESS --testnet-magic 8
```