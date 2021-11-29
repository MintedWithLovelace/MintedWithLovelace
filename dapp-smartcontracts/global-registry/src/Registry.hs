{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Registry
  ( registryScript
  , registryScriptShortBs
  ) where

import           Codec.Serialise
import           Plutus.V1.Ledger.Contexts
import qualified PlutusTx
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Prelude                  hiding (($))
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.ByteString.Short    as SBS
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified Ledger.Typed.Scripts     as Scripts
import           PlutusTx.Prelude         as P hiding (Semigroup (..), unless)
import           Ledger                   hiding (singleton)

data RegistryParams = RegistryParams
    { mwlPubKey :: PubKeyHash
    }
PlutusTx.makeLift ''RegistryParams

registryValidator :: RegistryParams -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
registryValidator registry _ _ context = traceIfFalse "missing mwl pubkey" signedByMwl

    where
      info :: TxInfo
      info = scriptContextTxInfo context
      
      mwlSig :: PubKeyHash
      mwlSig = mwlPubKey registry

      signedByMwl :: Bool
      signedByMwl = txSignedBy info $ mwlSig

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType    Typed = BuiltinData
    type instance RedeemerType Typed = BuiltinData

typedValidator :: RegistryParams -> Scripts.TypedValidator Typed
typedValidator registry = Scripts.mkTypedValidator @Typed
    ($$(PlutusTx.compile [|| registryValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode registry)
    $$(PlutusTx.compile  [|| wrap        ||])
  where
    wrap = Scripts.wrapValidator @BuiltinData @BuiltinData

validator :: Plutus.Validator
validator = Scripts.validatorScript (typedValidator registry)
    where registry = RegistryParams { mwlPubKey = pubKeyHash "645b11c8bacc2c2df73a84a4fb7a68d5e4b186476a1fcf38cbcd5bfa" }

script :: Plutus.Script
script = Plutus.unValidatorScript validator

registryScriptShortBs :: SBS.ShortByteString
registryScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

registryScript :: PlutusScript PlutusScriptV1
registryScript = PlutusScriptSerialised registryScriptShortBs