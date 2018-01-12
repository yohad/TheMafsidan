{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
|]

instance Yesod App

<<<<<<< HEAD
mainPage = do
    [whamlet|
             <head>
               <h1>The Mafsidan
               <h2>The Realest of News
             <div>This is my personal website.
                  My goal is to make it a safe haven for my weird ideas.
                  The topics I think I'll talk about (but I'm not limiting my self) are Physics, Math, CS, Games, Music and Art.
            |]
    toWidget [cassius|
        div
            margin-left: 100x
            margin-right: 100x
        p
            background-color: #171f28
            color: #779ecd
=======
--getLogoR =
--   sendFile typeJpeg "~/Haskell/TheMafsidan/src/The Mafsidan.png"

footer = do
    [whamlet|<i>This site is 100% type safe|]
    toWidget [lucius|p { font-size: 11px }|]
      
mainPage = do
    [whamlet|
             <head>
               <img >
             <div class="text">
               <img src="../src/The Mafsidan.png" alt="The Realest of News">
               <h1>The Mafsidan
               <h2>The Realest of News
               <p>This is my personal website.
                  My goal is to make it a safe haven for my weird ideas.
                  The topics I think I'll talk about (but I'm not limiting my self) are Physics, Math, CS, Games, Music and Art.
               ^{footer}
            |]
    toWidget [cassius|
        .text
            width: 800px
            margin-left: auto
            margin-right: auto
            background-color: #171f28
            color: #779ecd
        body
            background-color: #ffb347
        img
            vertical-align: middle
>>>>>>> git broke a bit
    |]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "The Mafsidan"
    toWidget mainPage

main :: IO ()
main = warp 3000 App
