
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Policies.MintEmulator where

-- Plutus modules
import qualified Plutus.Trace.Emulator as Emulator
import Control.Monad.Freer.Extras      as Extras
import Data.Default                 (Default (..))
import Data.Functor                 (void)
import Plutus.Trace
import Wallet.Emulator.Wallet                          as Wallet
import qualified Ledger.TimeSlot as TimeSlot

-- Our Offchain code
import qualified Policies.NftOffChain       as NftOffChain
import qualified Policies.Tokens       as Ts

test :: IO ()
test = Emulator.runEmulatorTraceIO trace1

trace1 :: Emulator.EmulatorTrace ()
trace1 = do
    -- let tn = "P2PNft"
    h1 <- Emulator.activateContractWallet (Wallet.knownWallet 1) NftOffChain.endpoints
    h2 <- Emulator.activateContractWallet (Wallet.knownWallet 2) NftOffChain.endpoints
    -- h3 <- Emulator.activateContractWallet (Wallet.knownWallet 3) OffChain.endpoints

    Emulator.callEndpoint @"Mint" h1 $ NftOffChain.NftParams {
          NftOffChain.npAddress = Wallet.mockWalletAddress $ knownWallet 2
        , NftOffChain.npTName  = Ts.borrowerNftTn
        , NftOffChain.npAmount  = 1
    }

    -- void $ waitNSlots 2
    -- Emulator.callEndpoint @"Mint" h2 $ NftOffChain.NftParams {
    --       NftOffChain.npAddress = Wallet.mockWalletAddress $ knownWallet 2
    --     , NftOffChain.npTName  = NftOffChain.tokenName
    --     , NftOffChain.npAmount  = 1
    --     }

    s <- waitNSlots 2
    Extras.logInfo $ "reached " ++ show s