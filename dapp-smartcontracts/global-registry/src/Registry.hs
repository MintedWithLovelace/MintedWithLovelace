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

data RegistryDatum = RegistryDatum
    { creatorPubKey :: PubKeyHash
    , creatorRoyalty :: !Integer
    }

PlutusTx.unstableMakeIsData ''RegistryDatum

{-# INLINABLE registryValidator #-}
registryValidator :: RegistryDatum -> () -> ScriptContext -> Bool
registryValidator dat () ctx = traceIfFalse "missing pubkey" signedByOwner

    where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      signedByOwner :: Bool
      signedByOwner = txSignedBy info $ creatorPubKey dat

data RegistryTyped
instance Scripts.ValidatorTypes RegistryTyped where
    type instance DatumType RegistryTyped = RegistryDatum
    type instance RedeemerType RegistryTyped = ()

typedValidator :: Scripts.TypedValidator RegistryTyped
typedValidator = Scripts.mkTypedValidator @RegistryTyped
    $$(PlutusTx.compile [|| registryValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @RegistryDatum @()

validator :: Plutus.Validator
validator = Scripts.validatorScript typedValidator

script :: Plutus.Script
script = Plutus.unValidatorScript validator

registryScriptShortBs :: SBS.ShortByteString
registryScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

registryScript :: PlutusScript PlutusScriptV1
registryScript = PlutusScriptSerialised registryScriptShortBs