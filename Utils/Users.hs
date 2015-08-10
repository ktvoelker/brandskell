module Utils.Users where

import Import
import Network.Wai (Request,requestHeaders)
import Data.Text.Encoding (decodeUtf8)

getUser :: Request -> Maybe Text
getUser r = let headers = requestHeaders r
            in decodeUtf8 `fmap` lookup "X-WEBAUTH-USER" headers
