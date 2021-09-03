{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.PlutusExample.MintingNFTScript 
    (apiExamplePlutusNFTMintingScript
    , mintingNFTScriptShortBs
    ) where

import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Codec.Serialise
import qualified Data.ByteString.Lazy   as LB
import qualified Data.ByteString.Short  as SBS
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

data NFTMintingParameters = NFTMintingParameters {
    tokenName :: TokenName,
    utxoId :: TxId,
    utxoIndex :: Integer
}

PlutusTx.makeLift ''NFTMintingParameters

{-# INLINABLE mkPolicy #-}
mkPolicy :: NFTMintingParameters -> BuiltinData -> ScriptContext -> Bool
mkPolicy NFTMintingParameters {..} _ ctx = 
                          traceError "Can't get tx info" info &&
                          traceError "Can't construct a utxoRef" utxoRef &&
                          traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    utxoRef :: TxOutRef
    utxoRef = TxOutRef utxoId utxoIndex

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == utxoRef) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(cs, tn, amt)] -> cs  == ownCurrencySymbol ctx && tn == tokenName && amt == 1
        _                -> False

policy :: NFTMintingParameters -> Scripts.MintingPolicy
policy params = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode params

plutusScript :: Script
plutusScript =
  unMintingPolicyScript (policy parameters)
    where 
        parameters = NFTMintingParameters {
            utxoId = TxId "96667140a738478f579a640a90c76a18d7c4d9d0d3cadb2e7860f82e9ddda584",
            utxoIndex = 0,
            tokenName = TokenName "OlgaNFT"
            }

validator :: Validator
validator = Validator $ plutusScript

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

apiExamplePlutusNFTMintingScript :: PlutusScript PlutusScriptV1
apiExamplePlutusNFTMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

mintingNFTScriptShortBs :: SBS.ShortByteString
mintingNFTScriptShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor
