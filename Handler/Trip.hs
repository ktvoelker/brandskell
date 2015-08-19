{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.Trip where

import Import
import Utils.Database
import Utils.Days
import Utils.Users

import qualified Hasql as H
import qualified Data.Text as T

getTripR :: Int -> Handler Html
getTripR tripId = do
    req <- waiRequest
    admin <- liftIO $ isAdmin req
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            Identity (num_entries :: Int) <- H.singleEx $ [H.stmt|
                    SELECT count(*)
                    FROM entries
                    WHERE trip_id = ?
                |] tripId
            if num_entries == 0
                then do
                    Identity (reason :: T.Text) <- H.singleEx $ [H.stmt|
                            SELECT reason
                            FROM trips
                            INNER JOIN reasons ON (trips.reason_id = reasons.id)
                            WHERE trips.id = ?
                        |] tripId
                    return ((reason,Nothing,Nothing),[])
                else do
                    ((r,st,en) :: (T.Text,Day,Day)) <- H.singleEx $ [H.stmt|
                            SELECT reason
                                 ,  min(entries.date_start) AS date_start
                                 ,  max(entries.date_end)   AS date_end
                            FROM trips
                            INNER JOIN entries ON (trips.id = entries.trip_id)
                            INNER JOIN reasons ON (trips.reason_id = reasons.id)
                            WHERE trips.id = ?
                            GROUP BY reason
                        |] tripId
                    (entries :: [(Int,Int,T.Text,Maybe T.Text,Day,Day,T.Text)])
                        <- H.listEx $ [H.stmt|
                            SELECT people.id
                                 , entries.id
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
                    (entryPics :: [[(Int,T.Text)]])
                        <- forM entries (\(_,entryId,_,_,_,_,_) ->
                            H.listEx $ [H.stmt|
                                    SELECT id
                                         , ext
                                    FROM images
                                    WHERE entry_id = ? AND img_type = 'doodle'
                                    ORDER BY id ASC
                                |] entryId)
                    return ((r,Just st,Just en), zip entries entryPics)
    case dbres of
        Left err -> error $ show err
        Right ((reason,mstart,mend),entries) ->
            defaultLayout $ do
                case (mstart,mend) of
                    (Just start, Just end) -> setTitle $
                                    toHtml (showDate start)
                                 ++ " - "
                                 ++ toHtml (showDate end)
                                 ++ " | Brandreth Guestbook"
                    _ -> setTitle $ "Empty Trip | Brandreth Guestbook"
                $(widgetFile "trip")
