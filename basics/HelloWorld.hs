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
/error ErrorR GET
/not-found NotFoundR GET
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
  errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
    setTitle "Request page not located"
    toWidget [hamlet|
<h1>Not Found
<p>We apologize for the inconvenience, but the requested page could not be located.
|]
  errorHandler other = defaultErrorHandler other

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Hello World!"
  myGenerateIds "Hello World!"
  toWidget [whamlet|
    <p>
      <ul>
        <li><a href=@{AlertsR}>Alerts
        <li><a href=@{PageR}>Page
        <li><a href=@{ErrorR}>Internal server error
        <li><a href=@{NotFoundR}>Not found
    
|]

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

getErrorR :: Handler ()
getErrorR = error "This is an error"

getNotFoundR :: Handler ()
getNotFoundR = notFound

main :: IO ()
main = warp 3000 HelloWorld