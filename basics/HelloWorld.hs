{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Exception (IOException, try)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import Yesod

data HelloWorld = HelloWorld

mkYesod
  "HelloWorld"
  [parseRoutes|
/ HomeR GET
/alerts AlertsR GET
/page PageR GET
/person/#Text PersonR GET
/year/#Integer/month/#Text/day/#Int DateR
/wiki/*Texts WikiR GET
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
  shouldLogIO HelloWorld src level = return True
  defaultLayout = myLayout
  errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
    setTitle "Request page not located"
    toWidget
      [hamlet|
<h1>Not Found
<p>We apologize for the inconvenience, but the requested page could not be located.
|]
  errorHandler other = defaultErrorHandler other

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Hello World!"
  myGenerateIds "Hello World!"
  $logDebug "Trying to read data file"
  edata <- liftIO $ try $ readFile "datafile.txt"
  case edata :: Either IOException String of
    Left e -> do
      $logError "Could not read datafile.txt"
      toWidget
        [whamlet|
          <p>
            <ul>
              <li><a href=@{AlertsR}>Alerts
              <li><a href=@{PageR}>Page
              <li><a href=@{ErrorR}>Internal server error
              <li><a href=@{NotFoundR}>Not found
          
        |]
    Right str -> do
      $logInfo "Reading of data file succeeded"
      let ls = lines str
      when (length ls < 5) $ $logWarn "Less than 5 lines of data"
      toWidget
        [whamlet|
                  <ol>
                      $forall l <- ls
                          <li>#{l}
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

-- http://localhost:3000/person/foo
getPersonR :: Text -> Handler Html
getPersonR name = defaultLayout [whamlet|<h1>Hello #{name}!|]

-- http://localhost:3000/year/2023/month/april/day/18
handleDateR :: Integer -> Text -> Int -> Handler Text -- text/plain
handleDateR year month day =
  return $
    T.concat [month, " ", T.pack $ show day, ", ", T.pack $ show year]

-- http://localhost:3000/wiki/lorem/ipsum
getWikiR :: [Text] -> Handler Text
getWikiR = return . T.unwords

main :: IO ()
main = warp 3000 HelloWorld
