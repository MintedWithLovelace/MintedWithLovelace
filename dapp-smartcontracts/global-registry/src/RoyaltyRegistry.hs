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

module RoyaltyRegistry
  ( valSerialised
  , valSBS
  ) where

import           Codec.Serialise
import qualified PlutusTx
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Prelude                  hiding (($))
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.ByteString.Short    as SBS
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified Ledger.Typed.Scripts     as Scripts
import           PlutusTx.Prelude         as P hiding (Semigroup (..), unless)
import           Ledger                   hiding (singleton)

{-# INLINABLE registryValidator #-}
registryValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
registryValidator _ _ _ = traceError "Registration is permanent"

scriptValidator :: Scripts.Validator
scriptValidator = mkValidatorScript $$(PlutusTx.compile [|| registryValidator ||])

valScript :: Plutus.Script
valScript = Plutus.unValidatorScript scriptValidator

valSBS :: SBS.ShortByteString
valSBS = SBS.toShort . LBS.toStrict $ serialise valScript

valSerialised :: PlutusScript PlutusScriptV1
valSerialised = PlutusScriptSerialised valSBS