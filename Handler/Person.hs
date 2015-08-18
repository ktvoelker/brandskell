{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.Person where

import Import
import Utils.Database
import Utils.Days
import Utils.Users

import qualified Hasql as H
import qualified Data.Text as T

getPersonR :: Int -> Handler Html
getPersonR personId = do
    req <- waiRequest
    checkIfAllowed req
    admin <- liftIO $ isAdmin req
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (person :: (T.Text,Maybe T.Text,Maybe T.Text,T.Text))
                <- H.singleEx $ [H.stmt|
                    SELECT people.name
                         , people.nickname
                         , people.csh_username
                         , sources.source
                    FROM people
                    INNER JOIN sources ON (sources.id = people.source)
                    WHERE people.id = ?
                |] personId
            (entries :: [(Int,Int,T.Text,Day,Day,T.Text)])
                <- H.listEx $ [H.stmt|
                    SELECT entries.trip_id
                         , entries.id
                         , reasons.reason
                         , entries.date_start
                         , entries.date_end
                         , entries.entry
                    FROM entries
                    INNER JOIN trips ON (entries.trip_id = trips.id)
                    INNER JOIN reasons ON (trips.reason_id = reasons.id)
                    WHERE entries.person_id = ?
                    ORDER BY date_start DESC
                |] personId
            (entryPics :: [[(Int,T.Text)]])
                <- forM entries (\(_,entryId,_,_,_,_) ->
                    H.listEx $ [H.stmt|
                            SELECT id
                                 , ext
                            FROM images
                            WHERE entry_id = ? AND img_type = 'doodle'
                            ORDER BY id ASC
                        |] entryId)
            return (person, (zip entries entryPics))
    case dbres of
        Left err -> error $ show err
        Right ((name,mnick,muser,source),entries) ->
            defaultLayout $ do
                setTitle $ toHtml (T.unpack name) ++ " | Brandreth Guestbook"
                $(widgetFile "person")
