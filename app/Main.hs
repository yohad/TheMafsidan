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
                     html
                       background-color: #b19cd9
                       
                     body
                       background-color: #ffb347
                       width: 800px
                       margin-left: auto
                       margin-right: auto
                     
                     img
                       display: block
                       margin-left: auto
                       margin-right: auto
                                        
                     a:link
                       color: #fa7777
                       text-decoration: none
                       font-weight: bold

                     .footer
                       font-style: italic
                       font-size: 11

                     ul.menu                     
                       list-style-type: none
                       margin: 0
                       padding: 0
                       overflow: hidden
                       background-color: #333
                       
                     li
                       float: left

                     li a
                       display: block
                       color: white
                       text-align: center
                       padding: 14px 16px
                       text-decoration: none

                     a.head:link
                       size: 30px
                       color: #fa7766
                       font-weight: bold

                     a.head:visited
                       size: 30px
                       color: #fa7766
                       font-weight: bold
                     |]
  withUrlRenderer
    [hamlet|
           <html>
               <head>
                   ^{pageHead pc}
               <body>
                   <img src=@{StaticR logo_png}>               
                   <article>
                       <ul .menu>
                           <li>
                             <a href=@{HomeR}>Home
                           <li>
                             <a href=@{PostsR}>Posts
                       ^{pageBody pc}
                       ^{footer}
           |]

footer = do
    [hamlet|<p .footer>This site is 100% type safe|]
      
mainPage = do
    [whamlet|
             <div .text>
               <h1>The Mafsidan
               <h2>The Realest of News
               <p>This is my personal website. My goal is to make it a safe
               haven for my weird ideas. The topics I think I'll talk about (but
               I'm not limiting my self) are Physics, Math, CS, Games, Music and
               Art.
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
  where
    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe (Left _) = Nothing
    eitherToMaybe (Right a) = Just a

getPostsR :: Handler Html
getPostsR = defaultLayout $ do
  posts <- liftIO $ listDirectory postsDir
  toWidget [whamlet|
                   <h1>Posts
                   $forall post <- posts
                     <a .head href=@{PostR post}>#{takeBaseName post}
                   |]
      
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "The Mafsidan"
    toWidget mainPage

main :: IO ()
main = do
    static@(Static settings) <- static "static"
    warp 3000 $ App static
