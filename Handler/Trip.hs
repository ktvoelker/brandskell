{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.Trip where

import Import
import Utils.Database
import Utils.Days

import qualified Hasql as H
import qualified Data.Text as T

getTripR :: Int -> Handler Html
getTripR tripId = do
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (trip :: (T.Text,Day,Day)) <- H.singleEx $ [H.stmt|
                    SELECT reason
                         ,  min(entries.date_start) AS date_start
                         ,  max(entries.date_end)   AS date_end
                    FROM trips
                    INNER JOIN entries ON (trips.id = entries.trip_id)
                    INNER JOIN reasons ON (trips.reason_id = reasons.id)
                    WHERE trips.id = ?
                    GROUP BY reason
                |] tripId
            (entries :: [(Int,T.Text,Maybe T.Text,Day,Day,T.Text)])
                <- H.listEx $ [H.stmt|
                    SELECT people.id
                         , people.name
                         , people.nickname
                         , entries.date_start
                         , entries.date_end
                         , entries.entry
                    FROM entries
                    INNER JOIN people ON (entries.person_id = people.id)
                    WHERE entries.trip_id = ?
                    ORDER BY people.name ASC
                |] tripId
            return (trip, entries)
    case dbres of
        Left err -> error $ show err -- TODO
        Right ((reason,start,end),entries) ->
            defaultLayout $ do
                setTitle $ toHtml (showDate start) ++ " - " ++ toHtml reason
                            ++ " | Brandreth Guestbook"
                $(widgetFile "trip")
