{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Text (Text)
import Yesod

data HelloWorld = HelloWorld

mkYesod
  "HelloWorld"
  [parseRoutes|
/ HomeR GET
/alerts AlertsR GET
/page PageR GET
|]

myLayout :: Widget -> Handler Html
myLayout widget = do
  pc <- widgetToPageContent widget
  withUrlRenderer
    [hamlet|
            $doctype 5
            <html>
                <head>
                    <title>#{pageTitle pc}
                    <meta charset=utf-8>
                    <style>body { font-family: verdana }
                    ^{pageHead pc}
                <body>
                    <article>
                        ^{pageBody pc}
        |]

myGenerateIds :: Text -> Widget
myGenerateIds pageTitle = do
  headerClass <- newIdent
  toWidget [hamlet|<h1 .#{headerClass}>#{pageTitle}|]
  toWidget [lucius| .#{headerClass} { color: green; } |]

instance Yesod HelloWorld where
  defaultLayout = myLayout

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Hello World!"
  myGenerateIds "Hello World!"
  toWidget [whamlet|Hello World!|]

alertsHead :: Widget
alertsHead = do
  setTitle "Alerts Page"
  toWidget [lucius| h1 { color: green; } |]
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
  toWidget
    [julius|
            $(function() {
                $("h1").click(function(){
                    alert("You clicked on the heading!");
                });
            });
        |]
  toWidgetHead
    [hamlet|
            <meta name=keywords content="some sample keywords">
        |]

alertsBody :: Widget
alertsBody = do
  toWidget
    [hamlet|
            <h1>Here's one way of including content
        |]
  [whamlet|<h2>Here's another |]
  toWidgetBody
    [julius|
            alert("This is included in the body itself");
        |]

getAlertsR :: Handler Html
getAlertsR = defaultLayout $ do
  alertsHead
  alertsBody

footer :: Widget
footer = do
    toWidget
        [lucius|
            footer {
                font-weight: bold;
                text-align: center
            }
        |]
    toWidget
        [hamlet|
            <footer>
                <p>That's all folks!
        |]

page :: Widget
page =
    [whamlet|
        <p>This is my page. I hope you enjoyed it.
        ^{footer}
    |]

getPageR :: Handler Html
getPageR = defaultLayout $ page

main :: IO ()
main = warp 3000 HelloWorld