--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified GHC.IO.Encoding as E
import           Data.List (sortBy)
import           Data.Ord (comparing)

--------------------------------------------------------------------------------
main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyllWith config $ do
    match "resources/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "fonts/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.md", "contact.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "writings/linux/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "writings/linux.md" $ do
        route $ setExtension "html"
        compile $ do
            linuxCtx <- tocCtx "writings/linux/*"
            pandocCompiler
              >>= loadAndApplyTemplate "templates/post.html"    linuxCtx
              >>= loadAndApplyTemplate "templates/toc.html"     linuxCtx
              >>= loadAndApplyTemplate "templates/default.html" linuxCtx
              >>= relativizeUrls

    match "writings.md" $ do
        route $ setExtension "html"
        compile $ do
            writingsCtx <- tocCtx "writings/*"
            pandocCompiler
              >>= loadAndApplyTemplate "templates/post.html"    writingsCtx
              >>= loadAndApplyTemplate "templates/toc.html"     writingsCtx
              >>= loadAndApplyTemplate "templates/default.html" writingsCtx
              >>= relativizeUrls

    create ["index.html"] $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "writings/**"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Contents"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

tocCtx :: Pattern -> Compiler (Context String)
tocCtx p = do
  is <- sortBy (comparing itemIdentifier) <$> loadAll p
  return $ listField "contents" postCtx (return is) `mappend` defaultContext

config :: Configuration
config = defaultConfiguration

