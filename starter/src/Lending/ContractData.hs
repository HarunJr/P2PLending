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

module Lending.ContractData where

import GHC.Generics (Generic)

import           Data.Aeson           (ToJSON, FromJSON)
import qualified Data.OpenApi.Schema                 as DataOpenApiSchema (ToSchema)
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


data ContractParam = ContractParam {
    creator         :: Ledger.PaymentPubKeyHash,
    borrower        :: Ledger.PaymentPubKeyHash,
    borrowersNftTn  :: LedgerApiV2.TokenName,     -- Borrowers token name encoding utxo to be consumed when minting token.
    -- stakeNft        :: ValueV1.AssetClass,
    borrowersNftCs  :: !LedgerApiV2.CurrencySymbol,
    -- loanAsset       :: ValueV1.AssetClass, -- Asset of required loan
    -- loanAmnt        :: Integer, -- Amount of requested loan
    collateral      :: Integer,
    requestExpiration :: LedgerApiV2.POSIXTime
    } deriving (P.Show)

    
PlutusTx.unstableMakeIsData ''ContractParam -- This is to instantiate the IsData class
PlutusTx.makeLift ''ContractParam
    
data RequestRedeemer = Borrow | Cancel deriving (P.Eq, P.Show, Generic, ToJSON, FromJSON, DataOpenApiSchema.ToSchema)

PlutusTx.unstableMakeIsData ''RequestRedeemer

instance Eq RequestRedeemer where
    {-# INLINABLE (==) #-}
    Borrow == Borrow = True
    Cancel == Cancel = True
    _ == _ = False

newtype RequestDatum = RequestDatum
    {
      action :: RequestRedeemer 

    } deriving (P.Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''RequestDatum 
-- PlutusTx.makeLift ''RequestDatum

data MintRedeemer = MintRedeemer
  {
    redeem          ::  RequestRedeemer, -- Borrow = Mint, Cancel = Burn
    utxo            ::  LedgerApiV2.TxOutRef
  } deriving  P.Show

PlutusTx.makeIsDataIndexed ''MintRedeemer [('MintRedeemer,0)]
-- PlutusTx.makeLift ''MintRedeemer

getUtxo :: LedgerApiV2.TxOutRef
getUtxo = LedgerApiV2.TxOutRef "b3af5e398b17925dad521424b7e5802ff09d9063ce446544bb0cb83628675122" 0
