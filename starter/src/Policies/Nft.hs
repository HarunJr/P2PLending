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
{-# LANGUAGE MultiParamTypeClasses #-}

module Policies.Nft where

import GHC.Generics (Generic)

import qualified Ledger                                          (PaymentPubKeyHash, unPaymentPubKeyHash)
import qualified Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV2
import qualified Plutus.Script.Utils.V2.Scripts      as UtilsScriptsV2
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts           as Contexts
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Prelude                                         as P
import qualified Plutus.V1.Ledger.Value                                as LValueV1 
import qualified Ledger.Address                                     as V1Address
import qualified Ledger.Ada               as Ada

data TokenParams = TokenParams
    {
      utxo :: LedgerApiV2.TxOutRef,
      name      :: LedgerApiV2.TokenName,
      borrower  :: Ledger.PaymentPubKeyHash
      -- requestAddress ::   V1Address.Address
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
mkTokenPolicy  token (MintRedeemer polarity adaAmount withdrawAmount) ctx =
  if polarity then validateTokenMint token adaAmount ctx else validateTokenBurn token ctx

validateTokenMint :: TokenParams -> Integer -> Contexts.ScriptContext -> Bool
validateTokenMint token collat ctx = 
  traceIfFalse "UTxO not consumed"   hasUTxO &&
  traceIfFalse "hasEnoughCollateral: Not enough collateral"  hasEnoughCollateral &&
  traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: Contexts.TxInfo
    info = Contexts.scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> Contexts.txInInfoOutRef i == utxo token) $ Contexts.txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case LValueV1.flattenValue $ Contexts.txInfoMint info of
        [(_, tn', amt')] -> tn' == name token && amt' == 1
        _                  -> False

    hasEnoughCollateral :: Bool
    hasEnoughCollateral = collat >= 1_000_000

validateTokenBurn :: TokenParams -> Contexts.ScriptContext -> Bool
validateTokenBurn token ctx =
  traceIfFalse "signedByBorrower: Not signed by borrower" signedByBorrower &&
  traceIfFalse "Wrong Burned Amount: Amount must be less than 1" checkBurntAmount
    where
      info :: Contexts.TxInfo
      info = Contexts.scriptContextTxInfo ctx

      signedByBorrower :: Bool
      signedByBorrower = Contexts.txSignedBy info $ Ledger.unPaymentPubKeyHash (borrower token)

      checkBurntAmount :: Bool
      checkBurntAmount = case LValueV1.flattenValue $ Contexts.txInfoMint info of
          [(_, tn', amt')] -> tn' == name token && amt' == (-1)
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
