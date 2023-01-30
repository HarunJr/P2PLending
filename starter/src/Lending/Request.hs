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

module Lending.Request 
  ( ContractParam (..)
  , RequestRedeemer (..)
  , RequestDatum (..)
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

import qualified Lending.Collateral as Collateral

data RequestDatum = RequestDatum {
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

PlutusTx.unstableMakeIsData ''RequestDatum 
PlutusTx.makeLift ''RequestDatum

data ContractParam = ContractParam {
    lenderNftCs    :: LedgerApiV2.CurrencySymbol,
    borrowersNftCs    :: LedgerApiV2.CurrencySymbol,
    collateralSc    :: V1Address.Address
  } deriving (P.Show)

PlutusTx.unstableMakeIsData ''ContractParam -- This is to instantiate the IsData class
PlutusTx.makeLift ''ContractParam
    
data RequestRedeemer = Borrow | Cancel | Lend deriving (P.Eq, P.Show, Generic, ToJSON, FromJSON, DataOpenApiSchema.ToSchema)

PlutusTx.unstableMakeIsData ''RequestRedeemer

instance Eq RequestRedeemer where
    {-# INLINABLE (==) #-}
    Borrow == Borrow = True
    Cancel == Cancel = True
    _ == _ = False


{-# INLINABLE mkRequestValidator #-}
mkRequestValidator :: ContractParam -> RequestDatum -> RequestRedeemer -> Contexts.ScriptContext -> Bool
mkRequestValidator cParam dat r ctx =  
  case r of 
    Borrow -> validateBorrowRequest cParam dat ctx
    Cancel -> validateCancelRequest cParam dat ctx
    Lend -> validateLendRequest cParam dat ctx

validateBorrowRequest :: ContractParam -> RequestDatum -> Contexts.ScriptContext -> Bool
validateBorrowRequest cParam dat ctx = 
  traceIfFalse "signedByBorrower: Not signed by borrower" signedByBorrower &&
  traceIfFalse "txHasOneScInputOnly: Tx Does not have one script input "  txHasOneScInputOnly &&
  traceIfFalse "NFT not minted" validateMint
  
  where
    txinfo :: Contexts.TxInfo
    txinfo = Contexts.scriptContextTxInfo ctx

    signedByBorrower :: Bool
    signedByBorrower = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (borrower dat)

    txHasOneScInputOnly :: Bool
    txHasOneScInputOnly =
      length (filter isJust $ Ledger.toValidatorHash . Contexts.txOutAddress . Contexts.txInInfoResolved <$> Contexts.txInfoInputs txinfo) == 1

    validateMint :: Bool
    validateMint = case ValueV1.flattenValue $ Contexts.txInfoMint txinfo of
      [(cs, tn, amt)] -> (cs == borrowersNftCs cParam) &&
                         (tn == borrowersNftTn dat) &&
                         (amt == 1)
      _               -> False

    -- deadlinePassed :: Bool
    -- deadlinePassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (requestExpiration dat)) (Contexts.txInfoValidRange txinfo)

validateCancelRequest :: ContractParam -> RequestDatum -> Contexts.ScriptContext -> Bool
validateCancelRequest cParam dat ctx =
  traceIfFalse "signedByBorrower: Wrong pubkeyhash" signedByBorrower &&
  traceIfFalse "deadlinePassed: Deadline not yet reached"  deadlinePassed &&
  traceIfFalse "validateBurn: Borrower NFT not burned" validateBurn 
  where
    txinfo :: Contexts.TxInfo
    txinfo = Contexts.scriptContextTxInfo ctx

    signedByBorrower :: Bool
    signedByBorrower = Contexts.txSignedBy txinfo $ Ledger.unPaymentPubKeyHash (borrower dat)

    deadlinePassed :: Bool
    deadlinePassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (requestExpiration dat)) (Contexts.txInfoValidRange txinfo)

    validateBurn :: Bool
    validateBurn = case ValueV1.flattenValue $ Contexts.txInfoMint txinfo of
        [(cs', tn', amt')]  ->  (cs' == borrowersNftCs cParam) &&
                                (tn' == borrowersNftTn dat) &&
                                (amt' == (-1))
        _                   -> False

validateLendRequest :: ContractParam -> RequestDatum -> Contexts.ScriptContext -> Bool
validateLendRequest cParam dat ctx =
  -- traceIfFalse "signedByBorrower: Wrong pubkeyhash" signedByLender &&
  traceIfFalse "deadlinePassed: Deadline not yet reached"  deadlinePassed &&
  traceIfFalse "containsRequiredCollateralAmount: Not Required Collateral Amount"  containsRequiredCollateralAmount &&
  traceIfFalse "validateBurn: Borrower NFT not burned" validateMint 
  where
    txinfo :: Contexts.TxInfo
    txinfo = Contexts.scriptContextTxInfo ctx

    deadlinePassed :: Bool
    deadlinePassed = LedgerIntervalV1.contains (LedgerIntervalV1.from (requestExpiration dat)) (Contexts.txInfoValidRange txinfo)

    validateMint :: Bool
    validateMint = case ValueV1.flattenValue $ Contexts.txInfoMint txinfo of
      [(cs, tn, amt)] -> (cs == lenderNftCs cParam) &&
                         (tn == lenderNftTn dat) &&
                         (amt == 1)
      _               -> False

    findDatumHash' :: PlutusTx.ToData a => a -> Contexts.TxInfo -> Maybe LedgerApiV2.DatumHash
    findDatumHash' datum info = Contexts.findDatumHash (LedgerApiV2.Datum $ LedgerApiV2.toBuiltinData datum) info

    expectedNewDatum :: LedgerApiV2.POSIXTime -> Collateral.CollateralDatum
    expectedNewDatum ld = Collateral.CollateralDatum {
        Collateral.borrowersNftTn        = borrowersNftTn dat
      , Collateral.borrower              = borrower dat
      , Collateral.collateral            = collateral dat
      , Collateral.collateralAmnt        = collateralAmnt dat
      , Collateral.lenderNftTn           = lenderNftTn dat
      , Collateral.interest              = interest dat
      , Collateral.interestAmnt          = interestAmnt dat
      , Collateral.loan                  = loan dat
      , Collateral.loanAmnt              = loanAmnt dat
      , Collateral.requestExpiration     = requestExpiration dat
      , Collateral.lendDate              = ld
      }

    -- isItToCollateral :: Bool
    -- isItToCollateral = any (\output -> Contexts.txOutAddress output == collateralSc cParam) (Contexts.txInfoOutputs txinfo)

    collateralAmount :: Contexts.TxOut ->Integer
    collateralAmount txo = assetClassValueOf (LedgerApiV2.txOutValue txo) (collateral dat)

    containsRequiredCollateralAmount :: Bool
    containsRequiredCollateralAmount = any (\txo -> collateralAmount txo >= collateralAmnt dat) (Contexts.txInfoOutputs txinfo) 

    getUpperBound :: Maybe LedgerApiV2.POSIXTime
    getUpperBound = case LedgerApiV2.ivTo (Contexts.txInfoValidRange txinfo) of
      LedgerApiV2.UpperBound (LedgerApiV2.Finite x) _ -> Just x
      _                       -> Nothing
    
data RequestDataTypes
instance V2UtilsTypeScripts.ValidatorTypes RequestDataTypes where
    type instance DatumType    RequestDataTypes = RequestDatum
    type instance RedeemerType RequestDataTypes = RequestRedeemer

--Boilerplate
requestTypedValidator :: ContractParam -> V2UtilsTypeScripts.TypedValidator RequestDataTypes
requestTypedValidator contractParam = V2UtilsTypeScripts.mkTypedValidator @RequestDataTypes
    ($$(PlutusTx.compile [|| mkRequestValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode contractParam)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = V2UtilsTypeScripts.mkUntypedValidator @RequestDatum @RequestRedeemer

validator :: ContractParam -> LedgerApiV2.Validator
validator = V2UtilsTypeScripts.validatorScript . requestTypedValidator
    
validatorHash :: ContractParam -> LedgerApiV2.ValidatorHash
validatorHash = V2UtilsTypeScripts.validatorHash . requestTypedValidator

-- script :: ContractParam -> Plutus.Script
-- script = Plutus.unValidatorScript . validatorHash

-- scriptAsCbor :: ContractParam -> LBS.ByteString
-- scriptAsCbor = serialise . validatorHash

-- request :: ContractParam -> PlutusScript PlutusScriptV1
-- request = PlutusScriptSerialised . requestShortBs

-- requestShortBs :: ContractParam -> SBS.ShortByteString
-- requestShortBs = SBS.toShort . LBS.toStrict . scriptAsCbor

address :: ContractParam -> V1Address.Address
address = V1Address.scriptHashAddress . validatorHash
