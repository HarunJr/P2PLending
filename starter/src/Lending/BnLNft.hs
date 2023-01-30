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
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lending.BnLNft where

import GHC.Generics (Generic)

import qualified Ledger                                          (PaymentPubKeyHash, unPaymentPubKeyHash)
import qualified Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV2
import qualified Plutus.Script.Utils.V2.Scripts      as UtilsScriptsV2
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts           as Contexts
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                                         as P
import qualified Plutus.V1.Ledger.Value                                as ValueV1 
-- import qualified Ledger.Address                                     as V1Address
-- import qualified Ledger.Ada               as Ada

data TokenParams = TokenParams
    {
      utxo              :: LedgerApiV2.TxOutRef,
      tokenName         :: LedgerApiV2.TokenName,
      beneficiary          :: Ledger.PaymentPubKeyHash
    } deriving (P.Eq, P.Ord, Generic, P.Show)

PlutusTx.unstableMakeIsData ''TokenParams
PlutusTx.makeLift ''TokenParams

data MintRedeemer = MintRedeemer
  {
    polarity        :: Bool, -- True = Mint, False = Burn
    adaAmount       :: Integer,  -- The total amount of Ada locked in the smart contract.   
    withdrawAmount  :: Integer  -- The amount of Ada to withdraw from the contract. Only used during burning
  } deriving  P.Show

PlutusTx.makeIsDataIndexed ''MintRedeemer [('MintRedeemer,0)]
PlutusTx.makeLift ''MintRedeemer

{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: TokenParams -> MintRedeemer -> Contexts.ScriptContext -> Bool
mkTokenPolicy  token MintRedeemer {..} ctx =
  if polarity then validateTokenMint token ctx else validateTokenBurn token ctx

validateTokenMint :: TokenParams -> Contexts.ScriptContext -> Bool
validateTokenMint token ctx = 
  traceIfFalse "UTxO not consumed"   hasUTxO &&
  traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> Contexts.txInInfoOutRef i == utxo token) $ Contexts.txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case ValueV1.flattenValue $ Contexts.txInfoMint info of
        [(_, tn', amt')] -> tn' == tokenName token && amt' == 1
        _                  -> False

validateTokenBurn :: TokenParams -> Contexts.ScriptContext -> Bool
validateTokenBurn token ctx =
  traceIfFalse "signedBybeneficiary: Not signed by borrower" signedByBeneficiary &&
  traceIfFalse "Wrong Burned Amount: Amount must be less than 1" checkBurntAmount
    where
      info :: Contexts.TxInfo
      info = Contexts.scriptContextTxInfo ctx

      signedByBeneficiary :: Bool
      signedByBeneficiary = Contexts.txSignedBy info $ Ledger.unPaymentPubKeyHash (beneficiary token)

      checkBurntAmount :: Bool
      checkBurntAmount = case ValueV1.flattenValue $ Contexts.txInfoMint info of
          [(_, tn', amt')] -> tn' == tokenName token && amt' == (-1)
          _                  -> False
      
{-# INLINABLE tokenPolicy #-}
tokenPolicy :: TokenParams -> LedgerApiV2.MintingPolicy
tokenPolicy token = LedgerApiV2.mkMintingPolicyScript $
        $$(PlutusTx.compile [|| wrap ||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode token
        where
          wrap token' = UtilsTypedScriptsMintingV2.mkUntypedMintingPolicy $ mkTokenPolicy token'

{-# INLINABLE tokenCurSymbol #-}
tokenCurSymbol :: TokenParams -> LedgerApiV2.CurrencySymbol
tokenCurSymbol = UtilsScriptsV2.scriptCurrencySymbol . tokenPolicy
-- tokenCurSymbol oref tn amt = scriptCurrencySymbol  (tokenPolicy oref tn amnt)

{-# INLINABLE assetClass #-}
assetClass :: LedgerApiV2.CurrencySymbol -> LedgerApiV2.TokenName -> ValueV1.AssetClass
assetClass s t = ValueV1.AssetClass (s, t)
