{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll

--------------
-- Contexts --
--------------

postCtx :: Context String
postCtx  = dateField "date" "%B %e, %Y"
 `mappend` defaultContext

archiveCtx :: [Item String] -> Context String
archiveCtx posts = listField "posts" postCtx (return posts)
         `mappend` constField "title" "Archives"
         `mappend` defaultContext

indexCtx :: Context String
indexCtx = constField "title" "Stephen Demos"
 `mappend` defaultContext

------------
-- Routes --
------------

images :: Rules ()
images = match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

css :: Rules ()
css = match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

posts :: Rules ()
posts = match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

archive :: Rules ()
archive = create ["archive.html"] $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" (archiveCtx posts)
            >>= loadAndApplyTemplate "templates/default.html" (archiveCtx posts)
            >>= relativizeUrls

index :: Rules ()
index = match "index.html" $ do
    route idRoute
    compile $ getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls

templates :: Rules ()
templates = match "templates/*" $ compile templateCompiler

----------
-- Main --
----------
main :: IO ()
main = hakyll $ do
    images
    css
    posts
    archive
    index
    templates
