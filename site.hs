--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import qualified GHC.IO.Encoding as E
import           Data.List (sortBy)
import           Data.Ord (comparing)
import           Data.Functor ((<&>))
--------------------------------------------------------------------------------
main :: IO ()
main = do
  E.setLocaleEncoding E.utf8
  hakyllWith config $ do

    match ("resources/*" .||. "images/*" .||. "js/*" .||. "fonts/*") $ do
      route   idRoute
      compile copyFileCompiler

    match ("css/*" .||. "css/highlight/*.css") $ do
      route   idRoute
      compile compressCssCompiler

    match (fromList ["about.md", "contact.md", "profile.md"]) $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

    tags <-       buildTags       postsGlob (fromCapture "tags/*.html") 
    categories <- buildCategories postsGlob (fromCapture "categories/*.html") 

    let postCtx' = postCtx `mappend` tagCtx tags `mappend` categoryCtx categories
    tagged tags       ("Posts tagged: " ++)     "templates/tag.html"
    tagged categories ("Posts categoried: " ++) "templates/category.html"

    posts postCtx' postsGlob
    toplevel postCtx' "posts/note.md" "posts/note/*"
    toplevel postCtx' "posts/linux.md" "posts/linux/*"
    toplevel postCtx' "writings.md" "posts/*"

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        let posts = loadAll "posts/**" >>= recentFirst
            archiveCtx =
              listField "posts" postCtx' posts
              `mappend` defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
          >>= loadAndApplyTemplate "templates/default.html" archiveCtx

    match "index.html" $ do
      route idRoute
      compile $ do
        tagListContent <- renderTagList tags
        categoryListContent <- renderTagList categories
        let posts = loadAll "posts/**" >>= recentFirst <&> take 5
            indexCtx =
              listField "posts" postCtx' posts
              `mappend` constField "title" "Contents"
              `mappend` constField "tagList" tagListContent
              `mappend` constField "categoryList" categoryListContent
              `mappend` defaultContext
        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postsGlob :: Pattern
postsGlob = "posts/**.md"

postCtx :: Context String
postCtx =
  dateField "date" "%Y-%m-%d" `mappend`
  defaultContext

tagCtx :: Tags -> Context String
tagCtx tags = tagsField "tagList" tags

categoryCtx :: Tags -> Context String
categoryCtx categories = tagsField "categoryList" categories

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

tagged :: Tags -> (String -> String) -> Identifier -> Rules ()
tagged tags mkTitle templatePath = 
  tagsRules tags $ \tag pattern -> do
    route idRoute
    compile $ do
      let posts = recentFirst =<< loadAll pattern
          ctx = constField "title" (mkTitle tag)
                `mappend` listField "posts" postCtx posts
                `mappend` defaultContext

      makeItem ""
          >>= loadAndApplyTemplate templatePath ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
