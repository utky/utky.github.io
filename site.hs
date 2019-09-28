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

    match ("resources/*" .||. "images/*" .||. "js/*" .||. "fonts/*") $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match (fromList ["about.md", "contact.md", "profile.md"]) $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    tags <- buildTags "writings/**" (fromCapture "tags/*.html") 

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title
                      `mappend` listField "posts" postCtx (return posts)
                      `mappend` defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    posts (postCtx `mappend` tagCtx tags) "writings/linux/*"

    toplevel postCtx "writings/linux.md" "writings/linux/*"

    toplevel postCtx "writings.md" "writings/*"

    match "index.html" $ do
      route idRoute
      compile $ do
        let posts = recentFirst =<< loadAll "writings/**"
            indexCtx =
              listField "posts" postCtx posts `mappend`
              constField "title" "Contents"            `mappend`
              defaultContext
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%Y-%m-%d" `mappend`
  defaultContext

tagCtx :: Tags -> Context String
tagCtx tags = tagsField "tags" tags

tocCtx :: Pattern -> Compiler (Context String)
tocCtx p = do
  let is = sortBy (comparing itemIdentifier) <$> loadAll p
  return $ listField "posts" postCtx is

config :: Configuration
config = defaultConfiguration

posts :: Context String -> Pattern -> Rules ()
posts ctx sources =
  match sources $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"    ctx
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls

toplevel :: Context String -> Pattern -> Pattern -> Rules ()
toplevel ctx source children =
  match source $ do
    route $ setExtension "html"
    compile $ do
      ctx' <- mappend ctx <$> tocCtx children
      pandocCompiler
        >>= loadAndApplyTemplate "templates/toc.html"     ctx'
        >>= loadAndApplyTemplate "templates/default.html" ctx'
        >>= relativizeUrls