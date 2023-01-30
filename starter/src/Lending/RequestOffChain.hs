
{-# LANGUAGE DataKinds #-} --Makes the kind system extensible
{-# LANGUAGE TemplateHaskell #-} --allows you to do type-safe compile-time meta-programming
{-# LANGUAGE NoImplicitPrelude   #-} --PlutusTx prelude has priority over Haskell prelude
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE TypeApplications    #-} --required to use custo datatypes
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE RecordWildCards #-} -- To allow notation like Getparams {..}

module Lending.RequestOffChain where

-- Haskell imports
import qualified Control.Monad                       as Monad (void)
import qualified GHC.Generics                        as GHCGenerics (Generic)
import qualified Data.Aeson                          as DataAeson (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
import qualified Prelude                  as P
import qualified Data.Map                 as Map
import qualified Data.Text                           as DataText (Text)
import           Data.Aeson           (ToJSON, FromJSON)

-- Plutus imports
import qualified PlutusTx
import PlutusTx.Prelude
import qualified Plutus.Contract          as PlutusContract
import qualified Ledger.Ada               as Ada
import qualified Ledger.Tx                as LedgerTx
import qualified Plutus.V2.Ledger.Api     as LedgerApiV2
import qualified Ledger                   (PaymentPubKeyHash, Value)
import qualified Text.Printf              as TextPrintf (printf)
import qualified Ledger.Constraints       as Constraints
import qualified Plutus.V1.Ledger.Scripts as ScriptsLedger
import qualified Plutus.V1.Ledger.Value   as ValueV1


import qualified Plutus.Script.Utils.V2.Scripts      as UtilsScriptsV2
import qualified Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV2

--Own modules
import qualified Lending.Request as OnChain         

mkPolicy :: () -> LedgerApiV2.ScriptContext -> Bool
mkPolicy () _ = True

policy :: LedgerApiV2.MintingPolicy
policy = LedgerApiV2.mkMintingPolicyScript $$(PlutusTx.compile [|| UtilsTypedScriptsMintingV2.mkUntypedMintingPolicy mkPolicy ||])

-- -- | Define Token
{-# INLINABLE tokenSymbol #-}
-- | The 'CurrencySymbol' of the 'Seal' currency.
tokenSymbol :: LedgerApiV2.CurrencySymbol
tokenSymbol = UtilsScriptsV2.scriptCurrencySymbol policy

{-# INLINABLE tokenName #-}
-- | The 'TokenName' of the 'Seal' currency.
tokenName :: LedgerApiV2.TokenName
tokenName = ValueV1.tokenName "HUSD" 

{-# INLINABLE tokenVAlueOf #-}
-- | A 'Value' with the given amount of Seal (the currency unit).
tokenVAlueOf :: Integer -> LedgerApiV2.Value
tokenVAlueOf = ValueV1.singleton tokenSymbol tokenName

-- data StartParams = StartParams
--     {     spBorrowersNftTn  :: !LedgerApiV2.TokenName     -- Borrowers token name encoding utxo to be consumed when minting token.
--         , spBorrower        :: !Ledger.PaymentPubKeyHash -- Who should receive the loan
--         -- , spLoanAsset       :: ValueV1.AssetClass -- Asset of required loan
--         -- , spLoanAmnt        :: !Integer -- Amount of requested loan
--         -- , spInterestAmnt    :: !Integer -- Amount of interest to be paid for loan
--         , spCollateralAmnt  :: !Integer -- Amount of collateral to be locked for loan
--         -- , spLoanDuration    :: !LedgerApiV2.POSIXTime -- How long till Lender gains rights to claim collateral. Once loan duration passes borrower must pay full interestAmt to claim collateral back.
--         , spRequestExpiration :: !LedgerApiV2.POSIXTime -- When does loan request expire. Lender should not be able to provide Loan past this time value.
--         , spRedeem          :: !Integer
        
--     } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)


data StartParams = StartParams
    { spCreator         :: !Ledger.PaymentPubKeyHash
    , spBorrower        :: !Ledger.PaymentPubKeyHash -- Who should receive the loan
    , spStake           :: !ValueV1.AssetClass
    , spBorrowersNftTn  :: !LedgerApiV2.TokenName     -- Borrowers token name encoding utxo to be consumed when minting token.
    , spCollateral      :: !Integer
    , spRequestExpiration :: !LedgerApiV2.POSIXTime -- When does loan request expire. Lender should not be able to provide Loan past this time value.
    , spRedeem          :: !OnChain.RequestRedeemer
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)

-- data GrabParams = GrabParams 
--     {     gpBorrowersNftTn  :: !LedgerApiV2.TokenName     -- Borrowers token name encoding utxo to be consumed when minting token.
--         , gpBorrower        :: Ledger.PaymentPubKeyHash -- Who should receive the loan
--         -- , gpLoanAsset       :: ValueV1.AssetClass -- Asset of required loan
--         -- , gpLoanAmnt      :: Integer -- Amount of requested loan
--         -- , gpInterestAmnt  :: Integer -- Amount of interest to be paid for loan
--         -- , gpCollateralAmnt  :: Integer -- Amount of collateral to be locked for loan
--         -- , gpLoanDuration    :: !LedgerApiV2.POSIXTime -- How long till Lender gains rights to claim collateral. Once loan duration passes borrower must pay full interestAmt to claim collateral back.
--         , gpRequestExpiration :: !LedgerApiV2.POSIXTime -- When does loan request expire. Lender should not be able to provide Loan past this time value.
--         , gpRedeem          :: !Integer
--     } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)


data GrabParams = GrabParams 
    { gpCreator  :: !Ledger.PaymentPubKeyHash
    , gpBorrower        :: !Ledger.PaymentPubKeyHash -- Who should receive the loan
    , gpStake           :: !ValueV1.AssetClass
    , gpBorrowersNftTn  :: !LedgerApiV2.TokenName     -- Borrowers token name encoding utxo to be consumed when minting token.
    , gpCollateral :: !Integer
    , gpRequestExpiration :: !LedgerApiV2.POSIXTime
    , gpRedeem    :: !OnChain.RequestRedeemer
    } deriving (GHCGenerics.Generic, DataAeson.ToJSON, DataAeson.FromJSON, DataOpenApiSchema.ToSchema)


type BeneSchema =
    PlutusContract.Endpoint "Start" StartParams
    PlutusContract..\/ PlutusContract.Endpoint "Grab" GrabParams

start :: PlutusContract.AsContractError e => StartParams -> PlutusContract.Contract w s e ()
start sp = do
    let p = OnChain.ContractParam{
            OnChain.creator = spCreator sp,
            OnChain.borrower = spBorrower sp,
            OnChain.stakeNft = spStake sp,
            OnChain.borrowersNftTn = spBorrowersNftTn sp,
            OnChain.collateral = spCollateral sp,
            OnChain.requestExpiration = spRequestExpiration sp
        }

        d = OnChain.RequestDatum {OnChain.action = spRedeem sp}
        v = Ada.lovelaceValueOf $ spCollateral sp
        txConstraints = Constraints.mustPayToOtherScript (OnChain.validatorHash p) (LedgerApiV2.Datum $ PlutusTx.toBuiltinData d) v
        lookups = Constraints.plutusV2OtherScript $ OnChain.validator p
        scriptAddress = OnChain.address p
        scriptHash = OnChain.validatorHash p

-- the final goal is to build and submit the transaction
    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.RequestDataTypes lookups txConstraints
    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Start Endpoint - Submited - Datum: %s - Value: %s ---------------------------" (P.show d) (P.show v)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "--------------------------- Script with Address: %s and Hash: %s ---------------------------" (P.show scriptAddress) (P.show scriptHash)

grab :: forall w s. GrabParams -> PlutusContract.Contract w s DataText.Text ()  
grab GrabParams{..} = do
    borrower <- PlutusContract.ownFirstPaymentPubKeyHash
    now <- PlutusContract.currentTime
    -- (oref, o, d@OnChain.RequestDatum{..}) <- findAuction bpCurrency bpToken
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Borrower: %s " (P.show gpBorrower)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Collateral: %s " (P.show gpCollateralAmount)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline: %s - Now: %s" (P.show gpRequestExpiration) (P.show now)
    -- PlutusContract.logInfo @P.String $ TextPrintf.printf "Reedeem: %s " (P.show gpRedeem)
    PlutusContract.logInfo @P.String $ TextPrintf.printf "Start Grab - Deadline: %s - Now: %s" (P.show gpRequestExpiration) (P.show now)
    if now > gpRequestExpiration
        then do 
            PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline PASSED - Deadline: %s - Now: %s" (P.show gpRequestExpiration) (P.show now)
            let param = OnChain.ContractParam {
                        OnChain.creator = gpCreator
                      , OnChain.borrower = borrower
                      , OnChain.stakeNft = gpStake
                      , OnChain.borrowersNftTn = gpBorrowersNftTn
                      , OnChain.collateral = gpCollateral
                      , OnChain.requestExpiration = gpRequestExpiration
                }
                r = gpRedeem
            utxos <- PlutusContract.utxosAt $ OnChain.address param
            if Map.null utxos
                then PlutusContract.logInfo @P.String $ "No utxos found"
                else PlutusContract.logInfo @P.String $ "utxos Available"
                
            maybeutxo <- findUtxoInValidator param gpRedeem --finds the utxos associated to the borrower that have valid deadline and redeemer
            case maybeutxo of
                Nothing -> PlutusContract.logInfo @P.String $ TextPrintf.printf "Wrong guess %s or not deadline reached %s and pubkey %s" (P.show maybeutxo) (P.show $ now) (P.show $ OnChain.borrower param)
                Just (oref, o) -> do
                    PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeem utxos %s - with timing now at %s:" (P.show oref) (P.show $ now)
                    let lookups = Constraints.unspentOutputs (Map.singleton oref o) P.<>
                                Constraints.plutusV2OtherScript (OnChain.validator param)
                        tx = Constraints.mustSpendScriptOutput oref (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
                            Constraints.mustValidateIn (LedgerApiV2.from now) P.<>
                            Constraints.mustPayToPubKey gpCreator (getTotalValuePay o)
                    submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.RequestDataTypes lookups tx
                    Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
                    PlutusContract.logInfo @P.String $ "collected gifts"

        else PlutusContract.logInfo @P.String $ TextPrintf.printf "Deadline not yet reached  - Deadline: %s - Now: %s" (P.show gpRequestExpiration) (P.show now)
            -- let param = OnChain.ContractParam {
            --             OnChain.borrowersNftTn = gpBorrowersNftTn,
            --             OnChain.borrower = gpBorrower,
            --             -- OnChain.loanAsset = gpLoanAsset,
            --             -- OnChain.loanAmnt = gpLoanAmnt,
            --             -- OnChain.interestAmnt = gpInterestAmnt,
            --             -- OnChain.collateralAmnt = gpCollateralAmnt,
            --             -- OnChain.loanDuration = gpLoanDuration,
            --             OnChain.requestExpiration = gpRequestExpiration
            --     }
            -- let param = OnChain.ContractParam {
            --         OnChain.creator = gpCreator
            --       , OnChain.borrower = borrower
            --       , OnChain.borrowersNftTn = gpBorrowersNftTn
            --       , OnChain.requestExpiration = gpRequestExpiration
            --     }
            --     -- r = OnChain.RequestRedeemer { OnChain.redeem = gpRedeem }
            --     r = gpRedeem
            -- utxos <- PlutusContract.utxosAt $ OnChain.address param
            -- if Map.null utxos
            --     then PlutusContract.logInfo @P.String $ "No utxos Available"
            --     else PlutusContract.logInfo @P.String $ "utxos Available"

            -- maybeutxo <- findUtxoInValidator param gpRedeem --finds the utxos associated to the borrower that have valid deadline and redeemer
            -- case maybeutxo of
            --     Nothing -> PlutusContract.logInfo @P.String $ TextPrintf.printf "Wrong guess %s or not deadline reached %s and pubkey %s" (P.show maybeutxo) (P.show $ now) (P.show $ OnChain.borrower param)
            --     Just (oref, o) -> do
            --         PlutusContract.logInfo @P.String $ TextPrintf.printf "Redeem utxos %s - with timing now at %s:" (P.show oref) (P.show $ now)
            --         let lookups = Constraints.unspentOutputs (Map.singleton oref o) P.<>
            --                     Constraints.plutusV2OtherScript (OnChain.validator param)
            --             tx = Constraints.mustSpendScriptOutput oref (ScriptsLedger.Redeemer $ PlutusTx.toBuiltinData r) P.<>
            --                 Constraints.mustValidateIn (LedgerApiV2.from now)-- P.<>
            --                 -- Constraints.mustPayToPubKey gpBorrower (getTotalValuePay o)
            --         submittedTx <- PlutusContract.submitTxConstraintsWith @OnChain.RequestDataTypes lookups tx
            --         Monad.void $ PlutusContract.awaitTxConfirmed $ LedgerTx.getCardanoTxId submittedTx
            --         PlutusContract.logInfo @P.String $ "collected gifts"


getDatum :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> Maybe OnChain.RequestDatum
getDatum (_, o) = do
    let 
        datHashOrDatum = LedgerTx._ciTxOutScriptDatum o

    LedgerApiV2.Datum e <- snd datHashOrDatum
    
    case (LedgerApiV2.fromBuiltinData e :: Maybe OnChain.RequestDatum) of    
        Nothing -> Nothing
        d -> d

checkUTXO :: (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut) -> OnChain.RequestRedeemer -> Bool
checkUTXO (oref,o) r = do
    case getDatum (oref,o) of
        Nothing -> False  
        Just OnChain.RequestDatum{..}
            | action == r -> True
            | otherwise  -> False

findUTXO :: [(LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut)] -> OnChain.RequestRedeemer -> (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUTXO [] _ = Nothing
findUTXO [(oref,o)] r = do
    if checkUTXO (oref, o) r then 
        return (oref, o)
    else 
        Nothing
findUTXO ((oref,o):xs) r
    | checkUTXO (oref ,o)  r = return (oref, o)
    | otherwise = findUTXO xs r

findUtxoInValidator :: OnChain.ContractParam -> OnChain.RequestRedeemer -> PlutusContract.Contract w s DataText.Text (Maybe (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut))
findUtxoInValidator gparam r = do
    utxos <- PlutusContract.utxosAt $ OnChain.address gparam
    let 
        xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
        out = findUTXO xs r
    return out

-- findUtxoInValidator :: OnChain.ContractParam -> OnChain.RequestRedeemer -> PlutusContract.Contract w s DataText.Text (LedgerApiV2.TxOutRef, LedgerTx.ChainIndexTxOut, OnChain.RequestDatum)
-- findUtxoInValidator gparam r = do
--     utxos <- PlutusContract.utxosAt $ OnChain.address gparam
--     let 
--         xs = [ (oref, o) | (oref, o) <- Map.toList utxos ]
--     case xs of
--         [(oref, o)] -> case LedgerTx._ciTxOutDatum  o of
--             Left _          -> PlutusContract.throwError "datum missing"
--             Right (LedgerApiV2.Datum e) -> case PlutusTx.fromBuiltinData e of
--                 Nothing -> PlutusContract.throwError "datum has wrong type"
--                 Just d@OnChain.RequestDatum{..}
--                     | action == r -> return (oref, o, d)
--                     | otherwise          -> PlutusContract.throwError "auction token missmatch"
--         _           -> PlutusContract.throwError "auction utxo not found"

getTotalValuePay :: LedgerTx.ChainIndexTxOut -> Ledger.Value
getTotalValuePay o = do
    Ada.toValue $ (Ada.fromValue $ LedgerTx._ciTxOutValue o) `Ada.divide` 10
    -- return tValue

-- This puts all together. The select means to offer selection to the user. 
endpoints :: PlutusContract.Contract () BeneSchema DataText.Text ()
endpoints = PlutusContract.awaitPromise (start' `PlutusContract.select` grab') >> endpoints
    where 
        start' = PlutusContract.endpoint @"Start" start
        grab' = PlutusContract.endpoint @"Grab" $ grab