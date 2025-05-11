{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Development.Shake
  ( Action,
    Verbosity (..),
    shakeLintInside,
    shakeOptions,
    shakeVerbosity,
  )
import Development.Shake.Forward (shakeArgsForward)

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  liftIO $ putStrLn "Hello, World"

main :: IO ()
main = do
  let shOpts = shakeOptions {shakeVerbosity = Verbose, shakeLintInside = ["\\"]}
  shakeArgsForward shOpts buildRules
