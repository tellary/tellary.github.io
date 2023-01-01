--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Maybe              (fromJust)
import Data.Monoid             (mappend)
import Hakyll
import NextPrevPageContext     (nextPrevPageContext)
import YearMonthArchiveContext (yearMonthArchiveContext)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
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

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            let archiveCtx = yearMonthArchiveContext "posts/*"
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    create ["index.html"] $ do
        route idRoute
        compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let firstPost = head $ posts :: Item String
          url <- fmap fromJust . getRoute $ itemIdentifier firstPost
          makeItem $ Redirect url

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Pattern -> Context String
postCtx ptrn =
    nextPrevPageContext ptrn `mappend`
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
