{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Yesod

data NonHtml = NonHtml

mkYesod
  "NonHtml"
  [parseRoutes|
/ HomeR GET
|]

instance Yesod NonHtml

getHomeR = return $ object ["foo" .= "bar"]

main = warp 3000 NonHtml