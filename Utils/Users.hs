module Utils.Users where

import Import
import Network.Wai (Request,requestHeaders)
import Data.Text.Encoding (decodeUtf8)

getUser :: Request -> Maybe Text
getUser r = let headers = requestHeaders r
            in decodeUtf8 `fmap` lookup "X-WEBAUTH-USER" headers

isAdmin :: Request -> IO Bool
isAdmin req = do
    let username = getUser req
    conn <- getDbConn
    dbres <- H.session conn $ H.tx Nothing $ 
            (admin :: [Bool]) <- H.singleEx $ [H.stmt|
                    SELECT admin
                    FROM people
                    WHERE people.csh_username = ?
                |] username
    case dbres of
        Right [True] -> return True
        _ -> return False


restrictToAdmins :: Request -> Handler ()
restrictToAdmins req = do
    admin <- liftIO $ isAdmin req
    case admin of
        True -> return ()
        False -> permissionDenied "Only an admin can see that page"
