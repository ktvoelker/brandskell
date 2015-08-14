{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.People where

import Import
import Utils.Database

import qualified Hasql as H
import qualified Data.Text as T

groupify :: Int -> [a] -> [[a]]
groupify _ [] = []
groupify n xs = take n xs : groupify n (drop n xs)

getPeopleR :: Handler Html
getPeopleR = do
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (people :: [(Int,T.Text,Maybe T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT people.id
                         , people.name
                         , people.nickname
                    FROM people
                    ORDER BY people.name ASC
                |]
            return people
    case dbres of
        Left err -> error $ show err
        Right people ->
                defaultLayout $ do
                    let groups = groupify 4 people
                    setTitle "People | Brandreth Guestbook"
                    $(widgetFile "people")
