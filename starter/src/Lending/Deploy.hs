{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

module Lending.Deploy where

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..))
import           Codec.Serialise (serialise)
import qualified Data.Aeson                          as DataAeson
import           Data.Aeson.Text       (encodeToLazyText)

import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Short               as SBS
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
import qualified Ledger
import           Cardano.Api.Shelley ( fromPlutusData )
import qualified Data.Text.Lazy.IO   as LT
-- import           Plutus.V1.Ledger.Value                             as ValueV1

import qualified Lending.Request                as OnChain
import qualified Lending.BnLNft                as Nft
import qualified Lending.Collateral                as Collateral
import qualified Lending.FTokens                as FTokens

import qualified Ledger.Ada               as Ada

main :: IO()
main = do
    writeInitDatum
    writeContractDatum
    writeBorrowRedeemer
    writeCancelRedeemer
    writeLendRedeemer

    writeRedeemerMintNFT
    writeRedeemerBurnNFT

    _ <- writeRequestValidatorScript
    _ <- writeCollateralValidatorScript
    _ <- writeBorrowMintingPolicyScript
    _ <- writeLendMintingPolicyScript
    _ <- writeTokensValidatorScript

    fileContents <- readJSON $ basePath++"borrow-request-redeemer.json"
    print fileContents
    print $ "Borrower Nft Currency Symbol -------- "++ (show $ Nft.tokenCurSymbol borrowerTokenParams)
    print $ "Lender Nft Currency Symbol -------- "++ (show $ Nft.tokenCurSymbol lenderTokenParams)
    print $ "USDH Currency Symbol -------- "++ (show $ FTokens.curSymbol fTokensParams)
    print $ "Collateral Script Address -------- "++ (show $ Collateral.address collateralParams)
    print $ Nft.assetClass LedgerApiV2.adaSymbol LedgerApiV2.adaToken
    putStrLn $ " maxAdaValue--------"++(show maxAdaValue)++"\n collateralAmount--------"++(show collateralAmount)++" Lovelace"++
            "\n loanAmount--------"++(show $ loanAmount collateralAmount)++" USDH"++"\n interestAmount--------"++
            (show $ interestAmount (loanAmount collateralAmount) 5)++" USDH"

    return ()

basePath :: FilePath
basePath = "/home/harun/dev/cardano/plutus/emurgoProject/scripts/work/plutus-scripts/" 
    
dataToScriptData :: LedgerApiV2.Data -> ScriptData
dataToScriptData (LedgerApiV2.Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (LedgerApiV2.List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (LedgerApiV2.I n)         = ScriptDataNumber n
dataToScriptData (LedgerApiV2.B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeJSONFromPlutus :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSONFromPlutus file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . LedgerApiV2.toData 

writeValidator :: FilePath -> LedgerApiV2.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unValidatorScript

writeMintingValidator :: FilePath -> LedgerApiV2.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unMintingPolicyScript

-- toJsonString :: PlutusTx.ToData a => a -> LBS.ByteString
-- toJsonString = DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . PlutusTx.toData

writePlainTextJSON :: DataAeson.ToJSON a => FilePath -> a -> IO ()
writePlainTextJSON file = LT.writeFile file . encodeToLazyText 

writeDatumHash :: LedgerApiV2.DatumHash -> IO ()
writeDatumHash = writePlainTextJSON "/home/harun/dev/cardano/plutus/emurgoProject/scripts/work/plutus-scripts/request-datum.json"

writeInitDatum :: IO ()
writeInitDatum = writeJSON (basePath++"unit.json") ()

readJSON :: FilePath -> IO LBS.ByteString
readJSON = LBS.readFile

writeBorrowRedeemer :: IO ()
writeBorrowRedeemer = 
    let red = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Borrow 
    in writeJSONFromPlutus (basePath++"borrow-request-redeemer.json") red 

writeCancelRedeemer :: IO ()
writeCancelRedeemer = 
    let red = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Cancel 
    in writeJSONFromPlutus (basePath++"cancel-request-redeemer.json") red
    
writeLendRedeemer :: IO ()
writeLendRedeemer = 
    let red = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Lend 
    in writeJSONFromPlutus (basePath++"lend-request-redeemer.json") red 

writeRedeemerMintNFT :: IO ()
writeRedeemerMintNFT = 
    let red = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData $ Nft.MintRedeemer 
             {
                Nft.polarity = True     -- mint token
             ,  Nft.adaAmount = 50_000_000  -- ingored for NFT minting
             ,  Nft.withdrawAmount = 0  -- ignored during minting   
             }
    in writeJSONFromPlutus (basePath++"mint-nft-redeemer.json") red -- LBS.writeFile basePath++"redeemer-mint-nft.json" $ DataAeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ LedgerApiV2.toData red)

writeRedeemerBurnNFT :: IO ()
writeRedeemerBurnNFT = 
    let red = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData $ Nft.MintRedeemer 
             {
                Nft.polarity = False      -- burn token
             ,  Nft.adaAmount = 0    -- ingored for NFT burn
             ,  Nft.withdrawAmount = 0    -- ingored for NFT burn   
             }
    in writeJSONFromPlutus (basePath++"burn-nft-redeemer.json") red -- LBS.writeFile basePath++"redeemer-mint-nft.json" $ DataAeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ LedgerApiV2.toData red)

writeContractDatum :: IO ()
writeContractDatum = 
    let contributor = requestDatum
        d = PlutusTx.toBuiltinData contributor
    in writeJSON (basePath++"request-datum.json") d

-- toAda :: Integer -> Ada.Ada
-- toAda i = i / 1_000_000

-- toInteger :: Ada.Ada -> Integer 
-- toInteger i = i * 1_000_000

-- Virtual Fixed Ada price based on the 
-- Average high and low Ada USD price of all time.
-- TODO: Should be calculated daily
maxAdaValue :: Double
maxAdaValue = 0.487209213

currentAdaValue :: Double
currentAdaValue = 0.377838

-- Loan To Value Ratio or Average of maxAdaValue AND currentAdaValue
ltvr :: Double
ltvr = (maxAdaValue + currentAdaValue) / 2

collateralAmount :: Integer
collateralAmount = 1000_000_000

-- 1 Ada = 0.62 usd
-- 21 Ada = ?
loanAmount :: Integer -> Integer
loanAmount c = (round (fromIntegral c * ltvr) `div` 1_000_000)

-- interestAmount :: Integer -> Integer -> Integer
-- interestAmount c i = loanAmount c * round (fromIntegral i / 100)

interestAmount :: Integer -> Integer -> Integer
interestAmount l i = (l * i) `div` 100

requestDatum :: OnChain.RequestDatum
requestDatum =  OnChain.RequestDatum {   
    OnChain.borrowersNftTn = LedgerApiV2.TokenName "BorrowNFT",
    OnChain.borrower = Ledger.PaymentPubKeyHash "b944755479d14f4e3973bac4684add100a3ebdabc35a8f87e0c30f17",
    OnChain.collateral = Nft.assetClass LedgerApiV2.adaSymbol LedgerApiV2.adaToken,
    OnChain.collateralAmnt = collateralAmount,
    OnChain.lenderNftTn = LedgerApiV2.TokenName "LendNFT",
    OnChain.interest = Nft.assetClass LedgerApiV2.adaSymbol LedgerApiV2.adaToken,
    OnChain.interestAmnt = interestAmount (loanAmount collateralAmount) 5,
    OnChain.loan = Nft.assetClass fTokensCs (LedgerApiV2.TokenName "USDH"),
    OnChain.loanAmnt = loanAmount collateralAmount,
    OnChain.requestExpiration = 1674051429000,        
    OnChain.lendDate = 1674051429000        
}

fTokensCs :: Ledger.CurrencySymbol
fTokensCs = FTokens.curSymbol fTokensParams

fTokensParams :: FTokens.SignParam
fTokensParams = FTokens.SignParam {
    FTokens.beneficiary = Ledger.PaymentPubKeyHash "6dde623cf9cccc589d33172139ba09fa8274c962ea3b6521d084cfc9"
}

contractParams :: OnChain.ContractParam
contractParams =  OnChain.ContractParam {   
    OnChain.lenderNftCs = Nft.tokenCurSymbol lenderTokenParams,
    OnChain.borrowersNftCs = Nft.tokenCurSymbol borrowerTokenParams,
    OnChain.collateralSc = Collateral.address collateralParams
}

collateralParams :: Collateral.CollateralParam
collateralParams =  Collateral.CollateralParam {   
    Collateral.lenderNftCs = Nft.tokenCurSymbol lenderTokenParams,
    Collateral.borrowersNftCs = Nft.tokenCurSymbol borrowerTokenParams
}

borrowerTokenParams :: Nft.TokenParams
borrowerTokenParams =  Nft.TokenParams{   
    Nft.utxo = LedgerApiV2.TxOutRef "6242ed4816615147f2bb33318495181b0e21fda8095b60ac29b87b37d53afd44" 0,
    Nft.tokenName = LedgerApiV2.TokenName "BorrowNFT",
    Nft.beneficiary = Ledger.PaymentPubKeyHash "b944755479d14f4e3973bac4684add100a3ebdabc35a8f87e0c30f17"
}

lenderTokenParams :: Nft.TokenParams
lenderTokenParams =  Nft.TokenParams{   
    Nft.utxo = LedgerApiV2.TxOutRef "95f09ed9ded2a41b8ffd2a64fc6c204e2e841476f8a9c0edadb6de009e68435e" 0,
    Nft.tokenName = LedgerApiV2.TokenName "LendNFT",
    Nft.beneficiary = Ledger.PaymentPubKeyHash "6dde623cf9cccc589d33172139ba09fa8274c962ea3b6521d084cfc9"
}

writeRequestValidatorScript :: IO (Either (FileError ()) ())
writeRequestValidatorScript =  writeValidator (basePath++"Request.plutus") $ OnChain.validator $ contractParams

writeBorrowMintingPolicyScript :: IO (Either (FileError ()) ())
writeBorrowMintingPolicyScript = writeMintingValidator (basePath++"Borrow-Request-Minting.plutus") $ Nft.tokenPolicy $ borrowerTokenParams

writeLendMintingPolicyScript :: IO (Either (FileError ()) ())
writeLendMintingPolicyScript = writeMintingValidator (basePath++"Lend-Request-Minting.plutus") $ Nft.tokenPolicy $ lenderTokenParams

writeCollateralValidatorScript :: IO (Either (FileError ()) ())
writeCollateralValidatorScript =  writeValidator (basePath++"Collateral.plutus") $ Collateral.validator $ collateralParams

writeTokensValidatorScript :: IO (Either (FileError ()) ())
writeTokensValidatorScript =  writeMintingValidator (basePath++"Tokens-Minting.plutus") $ FTokens.policy $ FTokens.SignParam
    {
        FTokens.beneficiary = Ledger.PaymentPubKeyHash "6dde623cf9cccc589d33172139ba09fa8274c962ea3b6521d084cfc9"
    }
