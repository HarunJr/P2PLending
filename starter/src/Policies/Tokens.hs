
{-# LANGUAGE OverloadedStrings   #-}

module Policies.Tokens where

import qualified Plutus.V2.Ledger.Api       as LedgerApiV2
import qualified Plutus.V1.Ledger.Value     as LValueV1
import qualified Plutus.Script.Utils.V2.Scripts      as UtilsScriptsV2

import qualified Policies.Nft       as OnChain

-- | The 'TokenNames' of the Currencies.

{-# INLINABLE borrowerNftTn #-}
borrowerNftTn :: LedgerApiV2.TokenName
borrowerNftTn = LValueV1.tokenName "ANFT" 

{-# INLINABLE lenderNftTn #-}
lenderNftTn :: LedgerApiV2.TokenName
lenderNftTn = LValueV1.tokenName "ANFT" 


{-# INLINABLE assetClass #-}
assetClass :: LedgerApiV2.CurrencySymbol -> LedgerApiV2.TokenName -> LValueV1.AssetClass
assetClass s t = LValueV1.AssetClass (s, t)

{-# INLINABLE tokenCurSymbol #-}
tokenCurSymbol :: OnChain.TokenParams -> LedgerApiV2.CurrencySymbol
tokenCurSymbol = UtilsScriptsV2.scriptCurrencySymbol . OnChain.tokenPolicy



