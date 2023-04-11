{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod

data Links = Links

mkYesod
  "Links"
  [parseRoutes|
/ HomeR GET
/foo FooR GET
/bar BarR GET
|]

instance Yesod Links

getHomeR = defaultLayout [whamlet|<a href=@{FooR}>Go to page foo!|]
getFooR = defaultLayout [whamlet|<a href=@{BarR}>Go to page bar!|]
getBarR = defaultLayout [whamlet|<a href=@{HomeR}>Go home!|]

main = warp 3000 Links