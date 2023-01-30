{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Lending.Collateral 
  ( CollateralParam (..)
  , RequestRedeemer (..)
  , CollateralDatum (..)
  , validator
  , validatorHash
  , address
  , RequestDataTypes
  ) where

import           Data.Aeson           (ToJSON, FromJSON)
import GHC.Generics (Generic)


import           PlutusTx.Prelude hiding (Semigroup (..))
import qualified PlutusTx

import qualified Ledger                                         -- (PaymentPubKeyHash, unPaymentPubKeyHash)
import qualified Ledger.Address                                     as V1Address
import qualified Ledger.Typed.Scripts                               as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators    as V2UtilsTypeScripts
import qualified Plutus.V2.Ledger.Api                               as LedgerApiV2
import qualified Plutus.V1.Ledger.Interval                          as LedgerIntervalV1
import qualified Plutus.V2.Ledger.Contexts                          as Contexts
import           Plutus.V1.Ledger.Value                             as ValueV1
import qualified Prelude                                            as P
import qualified Data.OpenApi.Schema                                as DataOpenApiSchema (ToSchema)
-- import qualified Ledger.Ada               as Ada

data CollateralDatum = CollateralDatum {
    borrowersNftTn    ::  LedgerApiV2.TokenName,     -- Borrowers token name encoding utxo to be consumed when minting token.
    borrower          ::  Ledger.PaymentPubKeyHash, -- Who should receive the loan
    collateral        :: ValueV1.AssetClass, -- Amount of collateral.
    collateralAmnt    :: Integer, -- Amount of collateral.
    lenderNftTn       ::  LedgerApiV2.TokenName,     -- Lenders token name encoding utxo to be consumed when minting token.
    interest          :: ValueV1.AssetClass, -- Asset of interest to be paid.
    interestAmnt      :: Integer, -- Amount of interest to be paid for loan.
    loan              :: ValueV1.AssetClass, -- Asset of requested loan.
    loanAmnt          :: Integer, -- Amount of requested loan.
    requestExpiration ::  LedgerApiV2.POSIXTime, -- When does loan request expire. Lender should not be able to provide Loan past this time value.
    lendDate          ::  LedgerApiV2.POSIXTime -- upper bound of transaction is valid to submit range.
} deriving (P.Show, Generic, ToJSON, FromJSON, DataOpenApiSchema.ToSchema)

PlutusTx.unstableMakeIsData ''CollateralDatum 
PlutusTx.makeLift ''CollateralDatum

data CollateralParam = CollateralParam {
    lenderNftCs       :: LedgerApiV2.CurrencySymbol,
    borrowersNftCs    :: LedgerApiV2.CurrencySymbol
  } deriving (P.Show)

PlutusTx.unstableMakeIsData ''CollateralParam -- This is to instantiate the IsData class
PlutusTx.makeLift ''CollateralParam
    
data RequestRedeemer = Borrow | Cancel | Lend deriving (P.Eq, P.Show, Generic, ToJSON, FromJSON, DataOpenApiSchema.ToSchema)

PlutusTx.unstableMakeIsData ''RequestRedeemer

instance Eq RequestRedeemer where
    {-# INLINABLE (==) #-}
    Borrow == Borrow = True
    Cancel == Cancel = True
    _ == _ = False

{-# INLINABLE mkCollateralValidator #-}
mkCollateralValidator :: CollateralParam -> CollateralDatum -> RequestRedeemer -> Contexts.ScriptContext -> Bool
mkCollateralValidator _ dat _ ctx = 
    traceIfFalse "validateDebtAmnt: Tx Does not have one script input "  validateDebtAmnt
    where
    txinfo :: Contexts.TxInfo
    txinfo = Contexts.scriptContextTxInfo ctx

    getLoanAmnt :: Value -> Integer
    getLoanAmnt v = assetClassValueOf v (loan dat)

    getInterestAmnt :: Value -> Integer
    getInterestAmnt v = assetClassValueOf v (interest dat)

    validateDebtAmnt :: Bool
    validateDebtAmnt = any (\txo -> getLoanAmnt (LedgerApiV2.txOutValue txo) >= loanAmnt dat) (Contexts.txInfoOutputs txinfo) 

      
data RequestDataTypes
instance V2UtilsTypeScripts.ValidatorTypes RequestDataTypes where
    type instance DatumType    RequestDataTypes = CollateralDatum
    type instance RedeemerType RequestDataTypes = RequestRedeemer

--Boilerplate
requestTypedValidator :: CollateralParam -> V2UtilsTypeScripts.TypedValidator RequestDataTypes
requestTypedValidator cParam = V2UtilsTypeScripts.mkTypedValidator @RequestDataTypes
    ($$(PlutusTx.compile [|| mkCollateralValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode cParam)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = V2UtilsTypeScripts.mkUntypedValidator @CollateralDatum @RequestRedeemer

validator :: CollateralParam -> LedgerApiV2.Validator
validator = V2UtilsTypeScripts.validatorScript . requestTypedValidator
    
validatorHash :: CollateralParam -> LedgerApiV2.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash . requestTypedValidator

-- script :: CollateralParam -> Plutus.Script
-- script = Plutus.unValidatorScript . validatorHash

-- scriptAsCbor :: CollateralParam -> LBS.ByteString
-- scriptAsCbor = serialise . validatorHash

-- request :: CollateralParam -> PlutusScript PlutusScriptV1
-- request = PlutusScriptSerialised . requestShortBs

-- requestShortBs :: CollateralParam -> SBS.ShortByteString
-- requestShortBs = SBS.toShort . LBS.toStrict . scriptAsCbor

address :: CollateralParam -> V1Address.Address
address = V1Address.scriptHashAddress . validatorHash
