{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Generates github page contents with Hakyll.

-}
module Main where

import Data.Monoid (mappend)
import Text.Pandoc (ReaderOptions(..), WriterOptions(..))
import Hakyll

main :: IO ()
main = hakyll $ ghPageWith myConf

-- | Configuration datatype for ghpage
--
data MyConfiguration = MyConfiguration
    { -- | Number of recent posts that are available
      numberOfRecentPosts :: Int
    , -- | Reader options for pandoc
      readerOptions       :: ReaderOptions
    , -- | Writer options for pandoc
      writerOptions       :: WriterOptions
    , -- | Atom feed configuration
      atomFeed            :: FeedConfiguration
    } deriving (Show)

-- | Defaults for 'MyConfiguration'
--
myConf :: MyConfiguration
myConf = MyConfiguration
    { numberOfRecentPosts = 3
    , readerOptions       = defaultHakyllReaderOptions
    , writerOptions       = defaultHakyllWriterOptions
      { writerHtml5 = True }
    , atomFeed            = myFeedConf
    }

-- | Atom feed configuration.
--
myFeedConf :: FeedConfiguration
myFeedConf = FeedConfiguration
  { feedTitle       = "Warm fuzzy thing by 8c6794b6"
  , feedDescription = "Warm and fuzzy"
  , feedAuthorName  = "8c6794b6"
  , feedAuthorEmail = "8c6794b6@gmail.com"
  , feedRoot        = "http://8c6794b6.github.com"
  }

-- | Version of 'ghPage' which allows setting a config
--
ghPageWith :: MyConfiguration -> Rules ()
ghPageWith conf = do

    -- Tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Images, audios and static files
    ["favicon.ico"]            --> copy
    ["img/**", "images/**"]    --> copy
    ["audio/**"]               --> copy
    ["static/**", "files/**"]  --> copy
    ["js/**", "javascript/**"] --> copy

    -- CSS files
    ["css/*.css", "style/*.css", "stylesheets/*.css"] --> css

    -- Posts
    match "posts/*" $ do
        let postCtx = tagsField "prettytags" tags `mappend` myCtx
        route $ setExtension "html"
        compile $ pandocCompilerWith (readerOptions conf) (writerOptions conf)
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    -- Archive
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" myCtx (return posts) `mappend`
                    constField "title" "Archive" `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    -- Index
    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let lists = listField "posts" myCtx (return $ take nRecent posts)
                tags' = field "tagcloud" (const $ renderTagCloud 100 250 tags)
                title = constField "title" "Home"
                topCtx = lists `mappend` tags' `mappend` title `mappend`
                         defaultContext
            getResourceBody
                >>= applyAsTemplate topCtx
                >>= loadAndApplyTemplate "templates/default.html" topCtx
                >>= relativizeUrls

    -- 404
    create ["404.html"] $ do
        route idRoute
        compile $ makeItem ""
            >>= loadAndApplyTemplate "templates/404.html" myCtx
            >>= loadAndApplyTemplate "templates/default.html" myCtx
            >>= relativizeUrls

    -- Tagged posts
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged &#8216;" ++ tag ++ "&#8217;"
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html"
                    (constField "title" title `mappend`
                     listField "posts" myCtx (return posts) `mappend`
                     defaultContext)
                >>= loadAndApplyTemplate "templates/default.html"
                     (constField "title" title `mappend`
                      defaultContext)
                >>= relativizeUrls

    -- RSS
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAllSnapshots "posts/*" "content"
            sorted <- take 10 `fmap` recentFirst posts
            let ctx = bodyField "description" `mappend` myCtx
            renderRss (atomFeed conf) ctx sorted

    -- Templates
    match "templates/*" $ compile templateCompiler

  where
    -- Combinator
    xs --> f = mapM_ (flip match f) xs

    -- Completely static
    copy = route idRoute >> compile copyFileCompiler

    -- CSS directories
    css = route (setExtension "css") >> compile compressCssCompiler

    -- Number of most recent posts to show
    nRecent = numberOfRecentPosts conf

    -- Default context
    myCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext
