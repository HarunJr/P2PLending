{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NumericUnderscores #-}

module Policies.Deploy where

import           Cardano.Api
import           Cardano.Api.Shelley (PlutusScript (..), fromPlutusData)
import           Codec.Serialise (serialise)
import qualified Data.Aeson                          as DataAeson
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Short               as SBS
import qualified Plutus.V2.Ledger.Api                as LedgerApiV2
import qualified PlutusTx
import qualified Ledger

import qualified Policies.Nft as OnChain

main :: IO()
main = do 
    writeInitDatum
    writeRedeemerMintNFT
    writeRedeemerBurnNFT
    _ <- writeMyFirstValidatorScript

    fileContents <- readJSON $ basePath++"mint-nft-redeemer.json"
    print fileContents

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

readJSON :: FilePath -> IO LBS.ByteString
readJSON = LBS.readFile

writeInitDatum :: IO ()
writeInitDatum = writeJSON (basePath++"mint-unit.json") ()

writeValidator :: FilePath -> LedgerApiV2.MintingPolicy -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . LedgerApiV2.unMintingPolicyScript

writeJSONFromPlutus :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSONFromPlutus file = LBS.writeFile file . DataAeson.encode . scriptDataToJson ScriptDataJsonDetailedSchema . fromPlutusData . LedgerApiV2.toData 

writeRedeemerMintNFT :: IO ()
writeRedeemerMintNFT = 
    let red = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData $ OnChain.MintRedeemer 
             {
                OnChain.polarity = True     -- mint token
             ,  OnChain.adaAmount = 50_000_000  -- ingored for NFT minting
             ,  OnChain.withdrawAmount = 0  -- ignored during minting   
             }
    in writeJSONFromPlutus (basePath++"mint-nft-redeemer.json") red -- LBS.writeFile basePath++"redeemer-mint-nft.json" $ DataAeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ LedgerApiV2.toData red)

writeRedeemerBurnNFT :: IO ()
writeRedeemerBurnNFT = 
    let red = LedgerApiV2.Redeemer $ PlutusTx.toBuiltinData $ OnChain.MintRedeemer 
             {
                OnChain.polarity = False      -- burn token
             ,  OnChain.adaAmount = 0    -- ingored for NFT burn
             ,  OnChain.withdrawAmount = 0    -- ingored for NFT burn   
             }
    in writeJSONFromPlutus (basePath++"burn-nft-redeemer.json") red -- LBS.writeFile basePath++"redeemer-mint-nft.json" $ DataAeson.encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromPlutusData $ LedgerApiV2.toData red)

writeMyFirstValidatorScript :: IO (Either (FileError ()) ())
writeMyFirstValidatorScript = writeValidator (basePath++"Minting.plutus") $ OnChain.tokenPolicy $ OnChain.TokenParams
    {
        OnChain.utxo = LedgerApiV2.TxOutRef "b3af5e398b17925dad521424b7e5802ff09d9063ce446544bb0cb83628675122" 0,
        OnChain.borrower = Ledger.PaymentPubKeyHash "b944755479d14f4e3973bac4684add100a3ebdabc35a8f87e0c30f17",
        OnChain.name = LedgerApiV2.TokenName "ANFT"
    }