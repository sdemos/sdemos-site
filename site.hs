{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll

--------------
-- Contexts --
--------------

postCtx :: Context String
postCtx = defaultContext

postListCtx :: [Item String] -> Context String
postListCtx posts = listField "items" postCtx (return posts)
                 <> constField "title" "Posts"
                 <> defaultContext

projectListCtx :: [Item String] -> Context String
projectListCtx projects = listField "items" postCtx (return projects)
                       <> constField "title" "Project Write-ups"
                       <> defaultContext

indexCtx :: Context String
indexCtx = constField "title" "Stephen Demos"
        <> defaultContext

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

resume :: Rules ()
resume = match "resume.pdf" $ do
    route   idRoute
    compile copyFileCompiler

posts :: Rules ()
posts = match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/disqus.html"  postCtx
        >>= loadAndApplyTemplate "templates/title.html"   postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

postList :: Rules ()
postList = create ["posts.html"] $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        makeItem ""
            >>= loadAndApplyTemplate "templates/list.html"      (postListCtx posts)
            >>= loadAndApplyTemplate "templates/title.html"     (postListCtx posts)
            >>= loadAndApplyTemplate "templates/default.html"   (postListCtx posts)
            >>= relativizeUrls

projects :: Rules ()
projects = match "projects/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html"    postCtx
        >>= loadAndApplyTemplate "templates/disqus.html"  postCtx
        >>= loadAndApplyTemplate "templates/title.html"   postCtx
        >>= loadAndApplyTemplate "templates/default.html" postCtx
        >>= relativizeUrls

projectList :: Rules ()
projectList = create ["projects.html"] $ do
    route idRoute
    compile $ do
        projects <- recentFirst =<< loadAll "projects/*"
        makeItem ""
            >>= loadAndApplyTemplate "templates/list.html"      (projectListCtx projects)
            >>= loadAndApplyTemplate "templates/title.html"     (projectListCtx projects)
            >>= loadAndApplyTemplate "templates/default.html"   (projectListCtx projects)
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
    resume
    posts
    postList
    projects
    projectList
    index
    templates
