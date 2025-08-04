{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import System.IO (putStrLn)

main :: IO ()
main = do
    let port = 8080
    let rootDir = "_site"
    putStrLn $ "Starting server on http://localhost:" ++ show port
    putStrLn $ "Serving files from: " ++ rootDir
    run port $ staticApp (defaultWebAppSettings rootDir)
