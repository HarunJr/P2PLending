{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NumericUnderscores    #-}

module Week06.Oracle.Core
    ( Oracle (..)
    , OracleRedeemer (..)
    , oracleTokenName
    , oracleValue
    , oracleAsset
    , typedOracleValidator
    , oracleValidator
    , oracleAddress
    , OracleSchema
    , OracleParams (..)
    , runOracle
    , findOracle
    ) where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import qualified Ledger                    (CurrencySymbol, PaymentPubKeyHash, unPaymentPubKeyHash)
-- import           Ledger.Constraints        as Constraints
-- import qualified Ledger.Typed.Scripts      as Scripts
-- import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
-- import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..), Show (..), String)

import qualified Prelude
import qualified Plutus.V2.Ledger.Api      as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts                          as Contexts
import qualified Ledger.Address                                  as V1LAddress
import qualified Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2UtilsTypeScripts
import qualified Plutus.V1.Ledger.Value                          as V1Value
import qualified Ledger.Constraints       as Constraints
import qualified Ledger.Tx                as LedgerTx


-- data Oracle = Oracle
--     { oSymbol   :: !V1Value.CurrencySymbol
--     , oOperator :: !Ledger.PaymentPubKeyHash
--     , oFee      :: !Integer
--     , oAsset    :: !V1Value.AssetClass
--     } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

data Oracle = Oracle
    { oSymbol           :: !V1Value.CurrencySymbol -- NFT identifiying the Orcale
    , oTokenName        :: !V1Value.TokenName
    , oOperator         :: !Ledger.PaymentPubKeyHash
    , oFee              :: !Integer
    --    , oAsset    :: !AssetClass
    , oCurrency         :: !V1Value.CurrencySymbol -- asset that wants to be exchanged
    , oToken            :: !V1Value.TokenName
    , oExchange         :: !Integer        -- exchange rate
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)


PlutusTx.makeLift ''Oracle

data OracleRedeemer = Update | Use
    deriving Show

PlutusTx.unstableMakeIsData ''OracleRedeemer

-- {-# INLINABLE oracleTokenName #-}
-- oracleTokenName :: V1Value.TokenName
-- oracleTokenName = V1Value.TokenName emptyByteString

{-# INLINABLE oracleAsset #-}
oracleAsset :: Oracle -> V1Value.AssetClass
oracleAsset oracle = V1Value.AssetClass (oSymbol oracle, oTokenName oracle)

{-# INLINABLE oracleDatum #-}
oracleDatum :: LedgerTx.TxOut -> (LedgerApiV2.DatumHash -> Maybe LedgerApiV2.Datum) -> Maybe Oracle
oracleDatum o f = do
    dh      <- LedgerTx.txOutDatum o
    LedgerApiV2.Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE oracleValue #-}
oracleValue :: LedgerTx.TxOut -> (LedgerApiV2.DatumHash -> Maybe LedgerApiV2.Datum) -> Maybe Integer
oracleValue o f = oExchange <$> oracleDatum o f

{-# INLINABLE mkOracleValidator #-}
mkOracleValidator :: Oracle -> Integer -> OracleRedeemer -> Contexts.ScriptContext -> Bool
mkOracleValidator oracle x r ctx =
    traceIfFalse "token missing from input"  inputHasToken  &&
    traceIfFalse "token missing from output" outputHasToken &&
    case r of
        Update -> traceIfFalse "operator signature missing" (Contexts.txSignedBy info $ Ledger.unPaymentPubKeyHash (oOperator oracle)) &&
                  traceIfFalse "invalid output datum"       validOutputDatum
        Use    -> traceIfFalse "oracle value changed"       (outputDatum == Just x)              &&
                  traceIfFalse "fees not paid"              feesPaid
  where
    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx

    ownInput :: LedgerTx.TxOut
    ownInput = case Contexts.findOwnInput ctx of
        Nothing -> traceError "oracle input missing"
        Just i  -> LedgerApiV2.txInInfoResolved i

    inputHasToken :: Bool
    inputHasToken = V1Value.assetClassValueOf (LedgerTx.txOutValue ownInput) (oracleAsset oracle) == 1

    ownOutput :: LedgerTx.TxOut
    ownOutput = case Contexts.getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one oracle output"

    outputHasToken :: Bool
    outputHasToken = V1Value.assetClassValueOf (LedgerTx.txOutValue  ownOutput) (oracleAsset oracle) == 1

    outputDatum :: Maybe Integer
    outputDatum = oracleValue ownOutput (`Contexts.findDatum` info)

    validOutputDatum :: Bool
    validOutputDatum = isJust outputDatum

    feesPaid :: Bool
    feesPaid =
      let
        inVal  = LedgerTx._ciTxOutValue ownInput
        outVal = LedgerTx._ciTxOutValue ownOutput
      in
        outVal `V1Value.geq` (inVal <> Ada.lovelaceValueOf (oFee oracle))

data Oracling
instance V2UtilsTypeScripts.ValidatorTypes Oracling where
    type instance DatumType Oracling = Integer
    type instance RedeemerType Oracling = OracleRedeemer

typedOracleValidator :: Oracle -> V2UtilsTypeScripts.TypedValidator Oracling
typedOracleValidator oracle = V2UtilsTypeScripts.mkTypedValidator @Oracling
    ($$(PlutusTx.compile [|| mkOracleValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = V2UtilsTypeScripts.mkUntypedValidator @Integer @OracleRedeemer

oracleValidator :: Oracle -> LedgerApiV2.Validator
oracleValidator = V2UtilsTypeScripts.validatorScript . typedOracleValidator

oracleAddress :: Oracle -> V1LAddress.Address
oracleAddress = V1LAddress.scriptHashAddress . oracleValidator

data OracleParams = OracleParams
    { opFees      :: !Integer
    , opSymbol    :: !V1Value.CurrencySymbol
    , opToken     :: !V1Value.TokenName
    , opExchange  :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

{-# INLINABLE oracleSymbol #-}
oracleSymbol :: V1Value.CurrencySymbol
oracleSymbol = V1Value.currencySymbol "dcddcaa"

{-# INLINABLE oracleTokenName #-}
oracleTokenName :: V1Value.TokenName
oracleTokenName = V1Value.tokenName "oracle"


startOracle :: forall w s. OracleParams -> Contract w s Text Oracle
startOracle op = do
    pkh <- Contract.ownPaymentPubKeyHash
    -- osc <- mapError (pack . show) (Currency.mintContract pkh [(oracleTokenName, 1)] :: Contract w s e ())
    -- let cs     = V1Value.currencySymbol osc
    let oracle = Oracle
            {     oSymbol       = oracleSymbol
                , oTokenName    = oracleTokenName
                , oCurrency     = opSymbol op
                , oOperator     = pkh
                , oFee          = opFees op
                , oExchange     = opExchange op
            }
    logInfo @String $ "started oracle " ++ show oracle
    return oracle

updateOracle :: forall w s. Oracle -> Integer -> Contract w s Text ()
updateOracle oracle x = do
    m <- findOracle oracle
    let 
        oracleNew = oracle { oExchange = x }
    -- I am assuming the wallet already own the oracle NFT
        c = Constraints.mustPayToTheScript oracleNew $ V1Value.assetClassValue (oracleAsset oracle) 1 <> lovelaceValueOf 2_000_000
    case m of
        Nothing -> do
            ledgerTx <- submitTxConstraints (typedOracleValidator oracle) c
            awaitTxConfirmed $ LedgerTx.getCardanoTxId ledgerTx
            logInfo @String $ "set initial oracle value to " ++ show x
        Just (oref, o,  _) -> do
            let lookups = Constraints.unspentOutputs (Map.singleton oref o)     <>
                          Constraints.plutusV1TypedValidatorLookups  (typedOracleValidator oracle) <>
                          Constraints.plutusV2OtherScript  (oracleValidator oracle)
                tx      = c <> Constraints.mustSpendScriptOutput oref (LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData Update)
            ledgerTx <- submitTxConstraintsWith @Oracling lookups tx
            awaitTxConfirmed $ LedgerTx.getCardanoTxId ledgerTx
            logInfo @String $ "updated oracle value to " ++ show x
    tell $ Last $ Just oracleNew

findOracle :: forall w s. Oracle -> Contract w s Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut, Oracle))
findOracle oracle = do
    utxos <- Map.filter f <$> Contract.utxosAt (oracleAddress oracle)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            x <- oracleValue (LedgerTx.toTxOut o) $ \dh -> case _ciTxOutDatum o of
                                                                Left _ -> Nothing
                                                                Right x -> Just x

            return (oref, o, x)
        _           -> Nothing
  where
    f :: LedgerTx.ChainIndexTxOut -> Bool
    f o = V1Value.assetClassValueOf (LedgerTx._ciTxOutValue o) (oracleAsset oracle) == 1

type OracleSchema = Endpoint "update" Integer

runOracle :: OracleParams -> Contract (Last Oracle) OracleSchema Text ()
runOracle op = do
    oracle <- startOracle op
    tell $ Last $ Just oracle
    go oracle
  where
    go :: Oracle -> Contract (Last Oracle) OracleSchema Text a
    go oracle = do
        x <- endpoint @"update"
        updateOracle oracle x
        go oracle