module Handler.Home where

import Import
import Utils.Users

getHomeR :: Handler Html
getHomeR = do
    req <- waiRequest
    checkIfAllowed req
    admin <- liftIO $ isAdmin req
    defaultLayout $ do
        setTitle "Brandreth Guestbook"
        $(widgetFile "homepage")
