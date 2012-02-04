{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-|
Module      : $Header$
Maintainer  : 8c6794b6@gmail.com
Stability   : experimental
Portability : portable

Generates github page contents with Hakyll.

-}
module Main where

import Control.Arrow ((>>>), arr)
import Data.Monoid
import Text.Pandoc (ParserState, WriterOptions)
import Hakyll

main :: IO ()
main = hakyll $ ghPageWith myConf

-- | Configuration datatype for ghpage
--
data MyConfiguration = MyConfiguration
    { -- | Number of recent posts that are available
      numberOfRecentPosts :: Int
    , -- | Parser state for pandoc, i.e. read options
      parserState         :: ParserState
    , -- | Writer options for pandoc
      writerOptions       :: WriterOptions
    , -- | Atom feed configuration
      atomFeed            :: Maybe FeedConfiguration
    } deriving (Show)

-- | Defaults for 'MyConfiguration'
--
myConf :: MyConfiguration
myConf = MyConfiguration
    { numberOfRecentPosts = 3
    , parserState         = defaultHakyllParserState
    , writerOptions       = defaultHakyllWriterOptions
    , atomFeed            = Just myFeedConf
    }

-- | Atom feed configuration.
--
myFeedConf :: FeedConfiguration
myFeedConf = FeedConfiguration
  { feedTitle       = "Warm fuzzy thing by 8c6794b6"
  , feedDescription = "Warm and fuzzy"
  , feedAuthorName  = "8c6794b6"
  , feedRoot        = "http://8c6794b6.github.com"
  }

-- | Version of 'ghPage' which allows setting a config
--
ghPageWith :: MyConfiguration -> Rules
ghPageWith conf = do

    -- Images, audios and static files
    ["favicon.ico"]            --> copy
    ["img/**", "images/**"]    --> copy
    ["audio/**"]               --> copy
    ["static/**", "files/**"]  --> copy
    ["js/**", "javascript/**"] --> copy

    -- CSS files
    ["css/*.css", "style/*.css", "stylesheets/*.css"] --> css

    -- "Dynamic" content
    ["posts/*"] --> post

    -- Top-level pages
    ["*.markdown", "*.html", "*.rst", "*.lhs"] --> topLevel

    -- Tags
    create "tags" $ do
      requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- 'Tagged as' list
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
      >>> arr tagsMap
      >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- All templates
    ["templates/*"] --> template

    -- Rss is optional
    case atomFeed conf of
        Nothing -> return ()
        Just f  -> do
            match  "atom.xml" $ route idRoute
            create "atom.xml" $ requireAll_ "posts/*" >>> renderAtom f
            return ()

  where
    -- Useful combinator here
    xs --> f = mapM_ (\p -> match p $ f) xs

    -- Completely static
    copy = route idRoute >> compile copyFileCompiler

    -- CSS directories
    css = route (setExtension "css") >> compile compressCssCompiler

    -- Templates
    template = compile templateCompiler

    -- Posts
    post = do
      route $ setExtension "html"
      compile $ pageCompilerWith (parserState conf) (writerOptions conf)
        >>> renderTagsField "prettytags" (fromCapture "tags/*")
        >>> applyTemplateCompiler "templates/post.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Top-level pages
    topLevel = do
      route $ setExtension "html"
      compile $ pageCompilerWithFields (parserState conf)
        (writerOptions conf) id topLevelFields
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Add the fields we need to top-level pages
    topLevelFields = setFieldPostList recentFirst "allPosts"
      >>> requireA "tags"
        (setFieldA "tagcloud" (renderTagCloud tagIdentifier 100 200))
      >>> setFieldPostList (take nRecent . recentFirst) "recentPosts"
      >>> setFieldPostList chronological "chronologicalPosts"

    -- Create a post list based on ordering/selection
    setFieldPostList f k = setFieldPageList f
      "templates/post-item.html" k "posts/*"

    -- Number of most recent posts to show
    nRecent = numberOfRecentPosts conf

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"

    -- makeTagList :: String -> [Page String] -> Compiler () (Page String)
    makeTagList tag posts = constA (mempty, posts)
      >>> addPostList
      >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
      >>> applyTemplateCompiler "templates/posts.html"
      >>> applyTemplateCompiler "templates/default.html"
      >>> relativizeUrlsCompiler

    -- addPostList :: Compiler (Page String, [Page String]) (Page String)
    addPostList =
      setFieldA "posts" $
      arr (reverse . chronological)
      >>> require "templates/post-item.html" (\p t -> map (applyTemplate t) p)
      >>> arr mconcat
      >>> arr pageBody
