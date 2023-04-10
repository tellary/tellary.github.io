--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe              (fromJust)
import Data.Monoid             (mappend)
import Hakyll
import NextPrevPageContext     (nextPrevPageContext)
import YearMonthArchiveContext (yearMonthArchiveContext)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile copyFileCompiler
        -- compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx "posts/*")
            >>= loadAndApplyTemplate "templates/default.html" (postCtx "posts/*")
            >>= relativizeUrls

    create ["archives.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx = yearMonthArchiveContext "posts/*" `mappend`
                             constField "title" "Archives"
            makeItem ""
                >>= loadAndApplyTemplate "templates/archives.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    create ["index.html"] $ do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let firstPost = case posts of
                (fp:_) -> fp :: Item String
                [] -> error "No posts found"
          url <- fmap fromJust . getRoute $ itemIdentifier firstPost
          makeItem $ Redirect url

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Pattern -> Context String
postCtx ptrn =
    nextPrevPageContext ptrn `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

config
  = defaultConfiguration
  { deployCommand = "./site clean && ./site rebuild && find docs -maxdepth 1 | egrep -v '^docs$' | grep -v docs/CNAME | xargs rm -rf && cp -r _site/* docs/ && git commit -a -m \"Publish 'docs'\" && git push"
  }
