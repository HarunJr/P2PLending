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

module Policies.NftOffChain where

-- Haskell imports
import           Data.Aeson           (ToJSON, FromJSON)
import GHC.Generics (Generic)
import qualified Data.Map                 as Map
import qualified Prelude                                         as P
import qualified Data.Text                           as DataText (Text)
import           Data.Void              (Void)
import qualified Text.Printf              as TextPrintf (printf)
import Data.Functor                 (void)

-- Plutus imports
import qualified Ledger                              as Ledger
import qualified Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as UtilsTypedScriptsMintingV2
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified Plutus.V2.Ledger.Contexts           as LedgerContextsV2
import qualified PlutusTx
import           PlutusTx.Prelude                    hiding (unless)
import qualified Ledger.Address                                     as V1Address

import qualified Ledger.Typed.Scripts        as Scripts
import           Plutus.Contract             as Contract
import           Ledger.Constraints          as Constraints
import qualified Plutus.V1.Ledger.Value                                as LValueV1
-- import           PlutusTx.Prelude            hiding (Semigroup(..), unless)

import qualified Policies.Nft as OnChain 

data NftParams = NftParams
    {    
          npAddress :: V1Address.Address
        , npTName   :: LedgerApiV2.TokenName
        , npAmount  :: Integer
        
    } deriving (P.Eq, P.Ord, Generic, FromJSON, ToJSON, P.Show)

type NFTSchema = Contract.Endpoint "Mint" NftParams

mintToken :: NftParams -> Contract w NFTSchema DataText.Text ()
mintToken np = do
    Contract.logDebug @P.String $ TextPrintf.printf "started minting: %s" $ P.show np
    utxos <- utxosAt $ npAddress np
    ppkh <- Contract.ownPaymentPubKeyHash
    case Map.keys utxos of
        []       -> Contract.logError @P.String "no utxo found"
        oref : _ -> do
            Contract.logInfo @P.String $ TextPrintf.printf "--------------------------- Start Endpoint - Submited - Utxo: %s ---------------------------" (P.show oref)
            
            let tn      = npTName np
                amt     = npAmount np
                tp      = (OnChain.TokenParams oref tn ppkh)
                cs      = (OnChain.tokenCurSymbol tp)
            Contract.logInfo @P.String $ TextPrintf.printf "--------------------------- Do we get here? - PolicyParameters: %s --------------------------" (P.show tp)
            Contract.logInfo @P.String $ TextPrintf.printf "--------------------------- tokenCurSymbol: %s --------------------------" (P.show cs)
            Contract.logInfo @P.String $ TextPrintf.printf "--------------------------- tokenName: %s --------------------------" (P.show tn)
            Contract.logInfo @P.String $ TextPrintf.printf "--------------------------- Amount: %s --------------------------" (P.show amt)
            
            let val     = LValueV1.singleton cs tn amt
                lookups = Constraints.plutusV2MintingPolicy  (OnChain.tokenPolicy tp) P.<> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val P.<> Constraints.mustSpendPubKeyOutput oref
            unbalanced <- Contract.mkTxConstraints @Scripts.Any lookups tx
            Contract.logDebug @DataText.Text "Unbalanced"
            adjusted  <- Contract.adjustUnbalancedTx unbalanced
            Contract.logDebug @DataText.Text "adjusted"
            unsigned  <- Contract.balanceTx adjusted
            Contract.logDebug @DataText.Text "unsigned"
            signed    <- Contract.submitBalancedTx unsigned
            Contract.logDebug @DataText.Text "signed"                    
            -- ledgerTx <- Contract.submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ Ledger.getCardanoTxId signed
            Contract.logInfo @P.String $ TextPrintf.printf "minted %s" (P.show val)

endpoints :: Contract.Contract () NFTSchema DataText.Text ()
endpoints = mint' >> endpoints
    where 
        mint' = Contract.awaitPromise $ Contract.endpoint @"Mint" mintToken
