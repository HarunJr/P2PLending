#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds           #-}

module Lending.ShellScript where

import Control.Concurrent
import System.Process
import System.IO
import System.Exit
import Control.Monad (void)

readShellFile :: String -> IO ()
readShellFile s = do
    (_, Just hout, _, ph) <- createProcess $ (shell s)
        {
            std_out = CreatePipe
        }
    output <- hGetContents hout
    putStr output
    hClose hout

    void $ waitForProcess ph

basePath :: FilePath
basePath = "/home/harun/dev/cardano/plutus/emurgoProject/scripts/work/shell-scripts/"

walletBalance :: String -> IO ()
walletBalance s = readShellFile $ basePath ++ "balance.sh " ++ s

payToScript :: IO ()
payToScript = readShellFile $ basePath ++ "payToScript.sh "

    -- exitCode <- system $ " chmod u+x " ++ basePath ++ "/payToScript.sh"
    -- case exitCode of
    --     ExitSuccess -> putStrLn "Script ran successfully"
    --     ExitFailure n -> putStrLn $ "Script failed with exit code " ++ show n

    -- result <- readCreateProcess (shell $ "sudo chmod u+x " ++ basePath ++ "payToScript.sh") ""
    -- print result
    -- readShellFile $ " chmod u+x " ++ basePath ++ "payToScript.sh"
    -- readShellFile $ basePath ++ "payToScript.sh"

sendFromWallet :: IO ()
sendFromWallet = readShellFile $ basePath ++ "sendFromWallet.sh"

mintFromScript :: IO ()
mintFromScript = readShellFile $ basePath ++ "mintFromScript.sh"

burnFromScript :: IO ()
burnFromScript = readShellFile $ basePath ++ "burnFromScript.sh"


