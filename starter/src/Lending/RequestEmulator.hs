
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}


module Lending.RequestEmulator where

-- import qualified Data.Map                 as Map
import Ledger.Index

-- Plutus modules
import qualified Plutus.Trace.Emulator as Emulator
import Control.Monad.Freer.Extras      as Extras
import Data.Default                 (Default (..))
import Data.Functor                 (void)
import Plutus.Trace
import Wallet.Emulator.Wallet                          as Wallet
import qualified Ledger.TimeSlot as TimeSlot
import qualified Ledger.Address                                     as V1Address
import qualified Plutus.V2.Ledger.Api     as LedgerApiV2
import qualified Plutus.V1.Ledger.Value     as ValueV1
import qualified Ledger

-- Our Offchain code
import qualified Lending.RequestOffChain       as OffChain
import qualified Lending.Request       as OnChain
import qualified Policies.Tokens       as Token
import qualified Policies.Nft       as NftOnChain

test :: IO ()
test = Emulator.runEmulatorTraceIO trace1

gameTokenCurrency :: LedgerApiV2.CurrencySymbol
gameTokenCurrency = "e95879b77e864364ac1974015ae8d28837c44f67a18e6a0bf95cd671"

gameTokenName :: LedgerApiV2.TokenName
gameTokenName = "ANFT"

trace1 :: Emulator.EmulatorTrace ()
trace1 = do
    h1 <- Emulator.activateContractWallet (Wallet.knownWallet 1) OffChain.endpoints
    h2 <- Emulator.activateContractWallet (Wallet.knownWallet 2) OffChain.endpoints
    h3 <- Emulator.activateContractWallet (Wallet.knownWallet 3) OffChain.endpoints

    -- Emulator.callEndpoint @"Start" h1 $ OffChain.StartParams {
        -- OffChain.spBorrowersNftTn   = Ts.borrowerNftTn,
    --     OffChain.spBorrower         = Wallet.mockWalletPaymentPubKeyHash $ knownWallet 2,
    --     -- OffChain.spLoanAsset        = Ts.assetClass getCurrencySymbol (Wallet.mockWalletAddress $ knownWallet 2) Ts.borrowerNftTn,
    --     -- OffChain.spLoanAmnt         = 20_000_000,
    --     -- OffChain.spInterestAmnt     = 10_000_000,
    --     OffChain.spCollateralAmnt   = 2_000_000,
    --     -- OffChain.spLoanDuration     = TimeSlot.slotToBeginPOSIXTime def 100,
    --     OffChain.spRequestExpiration  = TimeSlot.slotToBeginPOSIXTime def 30,
    --     OffChain.spRedeem           = 20
    -- }

    Emulator.callEndpoint @"Start" h1 $ OffChain.StartParams {
        OffChain.spCreator = Wallet.mockWalletPaymentPubKeyHash $ knownWallet 3,
        OffChain.spBorrower = Wallet.mockWalletPaymentPubKeyHash $ knownWallet 2,
        OffChain.spStake    = ValueV1.AssetClass (gameTokenCurrency, gameTokenName),
        OffChain.spBorrowersNftTn   = Token.borrowerNftTn,
        OffChain.spRequestExpiration = TimeSlot.slotToBeginPOSIXTime def 20,
        OffChain.spRedeem = OnChain.Borrow,
        OffChain.spCollateral = 50_000_000
    }   

    void $ waitNSlots 30
    Emulator.callEndpoint @"Grab" h2 $ OffChain.GrabParams {
        OffChain.gpCreator = Wallet.mockWalletPaymentPubKeyHash $ knownWallet 3,
        OffChain.gpBorrower = Wallet.mockWalletPaymentPubKeyHash $ knownWallet 2,
        OffChain.gpStake    = ValueV1.AssetClass (gameTokenCurrency, gameTokenName),
        OffChain.gpBorrowersNftTn   = Ts.borrowerNftTn,
        OffChain.gpCollateral = 50_000_000,
        OffChain.gpRequestExpiration = TimeSlot.slotToBeginPOSIXTime def 20,
        OffChain.gpRedeem = OnChain.Borrow
    }

    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s