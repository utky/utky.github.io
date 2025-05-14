#!/usr/bin/env cabal
{- cabal:
build-depends:
  base ^>=4.18.3.0
  , text
  , directory
  , filepath
-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text, pack, replace, unpack)
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))

-- Replace +++ with ---
convertMetadata :: Text -> Text
convertMetadata = replace "[taxonomies]" "" . replace "+++" "---"

-- Process a single file
processFile :: FilePath -> IO ()
processFile file = do
  content <- TIO.readFile file
  let updatedContent = convertMetadata content
  TIO.writeFile file updatedContent

-- Recursively process all Markdown files in a directory
processDirectory :: FilePath -> IO ()
processDirectory dir = do
  files <- listDirectory dir
  mapM_ processPath (map (dir </>) files)
  where
    processPath path = do
      isDir <- doesDirectoryExist path
      if isDir
        then processDirectory path
        else
          if takeExtension path == ".md"
            then processFile path
            else return ()

-- Main function
main :: IO ()
main = processDirectory "content"
