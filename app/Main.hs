{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Arrow (ArrowApply (app))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), ToJSON(..), Value (Object, String), toJSON, defaultOptions, fieldLabelModifier, genericParseJSON, genericToJSON, (.:), (.:?), (.!=))
import Data.Aeson.KeyMap (union)
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Data.List (isPrefixOf)
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
    writeFile'
  )
import Development.Shake.Classes (Binary)
import Development.Shake.FilePath (dropDirectory1, takeFileName, takeDirectory, (-<.>), (</>), replaceFileName)
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

-- | 記事の集合を持つ一覧ページ
data Section
  = Section
  { sectionTitle :: String,
    sectionContent :: T.Text,
    sectionDescription :: String,
    sectionPosts :: [Post],
    sectionSections :: [Section],
    sectionTags :: Maybe [Tag],
    sectionPath :: String
  }
  deriving (Generic, Eq, Ord, Show, Binary)

instance FromJSON Section where
  parseJSON = genericParseJSON defaultOptions
    { fieldLabelModifier = \field -> case field of
        "sectionTitle" -> "title"
        "sectionContent" -> "content"
        "sectionDescription" -> "description"
        "sectionPosts" -> "posts"
        "sectionSections" -> "sections"
        "sectionTags" -> "tags"
        "sectionPath" -> "path"
        _ -> field
    }

instance ToJSON Section where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = \field -> case field of
        "sectionTitle" -> "title"
        "sectionContent" -> "content"
        "sectionDescription" -> "description"
        "sectionPosts" -> "posts"
        "sectionSections" -> "sections"
        "sectionTags" -> "tags"
        "sectionPath" -> "path"
        _ -> field
    }

-- | 記事一般を表す基本的な構造
data Post
  = Post
  { postTitle :: String,
    postContent :: T.Text,
    postUrl :: String,
    postDate :: String,
    postTags :: Maybe [Tag]
  }
  deriving (Generic, Eq, Ord, Show, Binary)

instance FromJSON Post where
  parseJSON v = do
    obj <- parseJSON v
    title <- obj .: "title"
    content <- obj .: "content"  
    date <- obj .: "date"
    tags <- obj .:? "tags"
    url <- obj .:? "url" .!= ""  -- デフォルト値として空文字列を設定
    return $ Post title content url date tags

instance ToJSON Post where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = \field -> case field of
        "postTitle" -> "title"
        "postContent" -> "content"
        "postUrl" -> "url"
        "postDate" -> "date"
        "postTags" -> "tags"
        _ -> field
    }

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
        Post pTitle _ pUrl pDate pTags = s
        updatedPost = Post pTitle renderedContent pUrl pDate pTags
    return (updatedPost, renderedContent)
  return p
  where
    render t p = substitute t (toJSON p)

buildPost :: [Template] -> FilePath -> Action Post
buildPost templates src = do
  liftIO . putStrLn $ "Building post: " ++ src
  postContent <- liftIO . T.readFile $ src
  (postData :: Post) <- markdownToHTML' postContent
  let relativeDestPath = dropDirectory1 $ src -<.> "html"
      Post pTitle pContent _ pDate pTags = postData
      postWithUrl = Post pTitle pContent ("/" ++ relativeDestPath) pDate pTags
  renderedPost <- applyTemplates templates postWithUrl
  let Post _ renderedContent _ _ _ = renderedPost
  writeFile' (outputDirectory </> relativeDestPath) (T.unpack renderedContent)
  return renderedPost

buildPosts :: [Template] -> Action [Post]
buildPosts templates = do
  pPaths <- getDirectoryFiles "." ["content/posts//*.md"]
  let postPaths = filter (not . isIndexFile) pPaths
  forP postPaths (buildPost templates)
  where
    isIndexFile path = takeFileName path == "_index.md"

buildPostTemplates :: Action [Template]
buildPostTemplates = do
  let templatePaths = ["templates/post.html", "templates/base.html"]
  mapM compileTemplate' templatePaths

buildSectionTemplates :: Action [Template]
buildSectionTemplates = do
  let templatePaths = ["templates/section.html", "templates/base.html"]
  mapM compileTemplate' templatePaths

buildSection :: [Post] -> FilePath -> Action Section
buildSection allPosts indexPath = do
  liftIO . putStrLn $ "Building section: " ++ indexPath
  indexContent <- liftIO . T.readFile $ indexPath
  (postDataRaw :: Post) <- markdownToHTML' indexContent
  let sectionDir = takeDirectory indexPath
      sectionPosts = filter (isInExactDirectory sectionDir) allPosts
      sectionUrlPath = "/" ++ dropDirectory1 sectionDir
      -- PostからSectionを作成
      Post postTitle postContent _ postDate postTags = postDataRaw
      section = Section 
        { sectionTitle = postTitle
        , sectionContent = postContent
        , sectionDescription = ""
        , sectionPosts = sectionPosts
        , sectionSections = []  -- 後で設定
        , sectionTags = postTags
        , sectionPath = sectionUrlPath
        }
  liftIO . putStrLn $ "Section " ++ sectionUrlPath ++ " has " ++ show (length sectionPosts) ++ " posts"
  return section
  where
    isInExactDirectory sectionDir post = 
      let pUrl = postUrl post
          -- 投稿のURLからディレクトリパスを取得（例: "/posts/note/post.html" -> "/posts/note"）
          postDir = takeDirectory pUrl
          -- _index.mdのディレクトリから対応するURLディレクトリを生成
          -- "content/posts/note/_index.md" -> "/posts/note"
          sectionUrlDir = "/" ++ dropDirectory1 sectionDir
          -- 完全一致のみ（サブディレクトリは含まない）
      in postDir == sectionUrlDir

buildSections :: [Template] -> [Post] -> Action [Section]
buildSections templates allPosts = do
  indexPaths <- getDirectoryFiles "." ["content/posts//**/_index.md"]
  -- まず全セクションを構築
  sections <- forP indexPaths (buildSection allPosts)
  -- 親子関係を構築
  let sectionsWithChildren = map (addChildSections sections) sections
  -- 親子関係を持つセクションを再レンダリング
  forP sectionsWithChildren (reRenderSection templates)
  where
    addChildSections allSections section =
      let childSections = filter (isDirectChild section) allSections
      in section { sectionSections = childSections }
    
    isDirectChild parentSection childSection =
      let parentPath = sectionPath parentSection
          childPath = sectionPath childSection
      in parentPath /= childPath && takeDirectory childPath == parentPath

reRenderSection :: [Template] -> Section -> Action Section
reRenderSection templates section = do
  renderedSection@Section {sectionContent = renderedContent} <- applyTemplatesToSection templates section
  let sectionUrl = sectionPath section
      relativeDestPath = drop 1 sectionUrl ++ "/index.html"  -- "/posts/writings" -> "posts/writings/index.html"
  writeFile' (outputDirectory </> relativeDestPath) (T.unpack renderedContent)
  return renderedSection

applyTemplatesToSection :: [Template] -> Section -> Action Section
applyTemplatesToSection templates section = do
  (s, _) <- forAccumM section templates $ \sec t -> do
    let renderedContent = substitute t (toJSON sec)
        Section sTitle _ sDesc _ sSections sTags sPath = sec
        updatedSection = Section sTitle renderedContent sDesc [] sSections sTags sPath
    return (updatedSection, renderedContent)
  return s

outputDirectory :: FilePath
outputDirectory = "_site/"

copyStaticFiles :: Action ()
copyStaticFiles = do
  staticFiles <- getDirectoryFiles "." ["static//**"]
  _ <- forP staticFiles $ \src -> do
    let dest = outputDirectory </> dropDirectory1 src
    copyFileChanged src dest
    liftIO . putStrLn $ "Copying static file: " ++ src ++ " to " ++ dest
  return ()

-- | Specific build rules for the Shake system
--   defines workflow to build the website
buildRules :: Action ()
buildRules = do
  postTemplates <- buildPostTemplates
  sectionTemplates <- buildSectionTemplates
  allPosts <- buildPosts postTemplates
  _ <- buildSections sectionTemplates allPosts
  copyStaticFiles
  return ()

main :: IO ()
main = do
  let shOpts = shakeOptions {shakeVerbosity = Verbose, shakeLintInside = ["\\"]}
  shakeArgsForward shOpts buildRules
