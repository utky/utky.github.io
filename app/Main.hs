{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Arrow (ArrowApply (app))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, Value (Object, String), toJSON)
import Data.Aeson.KeyMap (union)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Data.Traversable (forAccumM)
import Development.Shake
  ( Action,
    Verbosity (Verbose),
    copyFileChanged,
    forP,
    getDirectoryFiles,
    liftIO,
    readFile',
    shakeLintInside,
    shakeOptions,
    shakeVerbosity,
    writeFile',
  )
import Development.Shake.Classes (Binary)
import Development.Shake.FilePath (dropDirectory1, (-<.>), (</>))
import Development.Shake.Forward (shakeArgsForward)
import GHC.Generics (Generic)
import Slick (compileTemplate', convert, markdownToHTML, markdownToHTML', substitute)
import Text.Mustache (Template)

---Config-----------------------------------------------------------------------

siteMeta :: SiteMeta
siteMeta =
  SiteMeta
    { siteAuthor = "Me",
      baseUrl = "https://utky.github.io/",
      siteTitle = "Hash λ Bye",
      twitterHandle = "ilyaletre",
      githubUser = "utky"
    }

outputFolder :: FilePath
outputFolder = "_site/"

-- Data models-------------------------------------------------------------------

withSiteMeta :: Value -> Value
withSiteMeta (Object obj) = Object $ union obj siteMetaObj
  where
    Object siteMetaObj = toJSON siteMeta
withSiteMeta _ = error "only add site meta to objects"

data SiteMeta
  = SiteMeta
  { siteAuthor :: String,
    baseUrl :: String, -- e.g. https://example.ca
    siteTitle :: String,
    twitterHandle :: String, -- Without @
    githubUser :: String
  }
  deriving (Generic, Eq, Ord, Show, ToJSON)

-- | Data for the index page
data IndexInfo
  = IndexInfo
  { posts :: [Post]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type Tag = String

-- | 記事一般を表す基本的な構造
data Post
  = Post
  { title :: String,
    content :: T.Text,
    date :: String,
    tags :: Maybe [Tag]
  }
  deriving (Generic, Eq, Ord, Show, FromJSON, ToJSON, Binary)

{-
記事データとテンプレート処理の関係についてメモ

記事データは原則的にテンプレートレンダリングが付随するものであると仮定できる。
したがって記事本体とフロントマターから解析されたデータ構造をテンプレート処理して単一のHTMLに変換するようなながれになる。
これらの手続きに必要な関数のシグニチャを考える。

data Html =
  Html
    {
      content :: T.Text,
      url :: String
    }
writeHtml :: Html -> FilePath -> Action ()

postToHtml :: HasTemplate t => SiteMeta -> t -> Action Html
postToHtml siteMeta post = do
  -- テンプレートは継承関係がある前提で、
  -- [Child Template, Parent Template, Grand Parent Template] のように取り出す。
  -- 子要素からテンプレートレンダリングした結果を親要素に当てはめていく。
  --
  templates <- liftIO . getTemplates $ t

-}

applyTemplates :: [Template] -> Post -> Action Post
applyTemplates templates post = do
  (p, _) <- forAccumM post templates $ \s t -> do
    let renderedContent = render t s
    return (post {content = renderedContent}, renderedContent)
  return p
  where
    render t p = substitute t (toJSON p)

buildPost :: [Template] -> FilePath -> Action Post
buildPost templates src = do
  liftIO . putStrLn $ "Building post: " ++ src
  postContent <- liftIO . T.readFile $ src
  postData <- markdownToHTML' postContent
  renderedPost@Post {content = renderedContent} <- applyTemplates templates postData
  let relativeDestPath = dropDirectory1 $ src -<.> "html"
  writeFile' (outputDirectory </> relativeDestPath) (T.unpack renderedContent)
  return renderedPost

buildPosts :: [Template] -> Action [Post]
buildPosts templates = do
  pPaths <- getDirectoryFiles "." ["content/posts//*.md"]
  forP pPaths (buildPost templates)

buildPostTemplates :: Action [Template]
buildPostTemplates = do
  let templatePaths = ["templates/page.html", "templates/base.html"]
  mapM compileTemplate' templatePaths

outputDirectory :: FilePath
outputDirectory = "_site/"

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  templates <- buildPostTemplates
  _ <- buildPosts templates
  return ()

main :: IO ()
main = do
  let shOpts = shakeOptions {shakeVerbosity = Verbose, shakeLintInside = ["\\"]}
  shakeArgsForward shOpts buildRules
