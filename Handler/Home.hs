module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "Brandreth Guestbook"
        $(widgetFile "homepage")
