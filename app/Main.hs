{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Yesod
import Yesod.Static
import Yesod.Markdown

import System.Directory
import System.FilePath.Posix ( takeBaseName
                             , takeFileName)

newtype App = App { getStatic :: Static }

staticFiles "static"
postsDir = "./static/posts/"

mkYesod "App" [parseRoutes|
/ HomeR GET
/static StaticR Static getStatic
/post/#String PostR GET
/posts PostsR GET
|]

instance Yesod App where
  defaultLayout = layout

layout :: Widget -> Handler Html
layout widget = do
  pc <- widgetToPageContent $ do
    widget
    toWidget [cassius|
                     body
                       background-color: #ffb347
                       width: 800px
                       margin-left: auto
                       margin-right: auto
                     img
                       vertical-align: middle
                     |]
  withUrlRenderer
    [hamlet|
           <html>
             <head>
               <img src=@{StaticR logo_png}>
               ^{pageHead pc}
             <body>
               <article>
                 ^{pageBody pc}
           |]

getPostsR :: Handler Html
getPostsR = defaultLayout $ do
  posts <- liftIO $ listDirectory postsDir
  toWidget [whamlet|
                   <h1>Post
                   $forall post <- posts
                     <a href=@{PostR post}>#{takeBaseName post}
                   |]

getPostR :: String -> Handler Html
getPostR name = defaultLayout $ do
  html <- liftIO $ markdownToHtml <$> markdownFromFile (postsDir ++ name)
  md <- liftIO $ markdownFromFile (postsDir ++ name)
  toWidget [whamlet|
                   $maybe doc <- eitherToMaybe html
                     #{doc}
                   $nothing
                     <p>Error, post does not exist.
                   |]

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a

footer = do
    [whamlet|<i>This site is 100% type safe|]
    toWidget [lucius|p { font-size: 11px }|]
      
mainPage = do
    [whamlet|
             <div class="text">
               <h1>The Mafsidan
               <h2>The Realest of News
               <p>This is my personal website.
                  My goal is to make it a safe haven for my weird ideas.
                  The topics I think I'll talk about (but I'm not limiting my self) are Physics, Math, CS, Games, Music and Art.
               ^{footer}
            |]
      
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "The Mafsidan"
    toWidget mainPage

main :: IO ()
main = do
    static@(Static settings) <- static "static"
    warp 3000 $ App static
