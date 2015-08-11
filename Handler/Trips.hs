{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.Trips where

import Import
import Utils.Database

import qualified Hasql as H
import qualified Data.Text as T

pageSize :: Int
pageSize = 10

getTripsR :: Int -> Handler Html
getTripsR page
    | page < 0 = getTripsR 0
    | otherwise = do
        dbres <- liftIO $ do
            conn <- getDbConn
            H.session conn $ H.tx Nothing $ do
                (trips :: [(Int,Day,Day,Int,T.Text)])
                    <- H.listEx $ [H.stmt|
                        SELECT trips.id            AS trip_id
                             , min(date_start)     AS date_start
                             , max(date_end)       AS date_end
                             , count(*)            AS num_attendees
                             , min(reasons.reason) AS reason
                        FROM trips
                        INNER JOIN entries ON (trips.id = entries.trip_id)
                        INNER JOIN reasons ON (trips.reason_id = reasons.id)
                        GROUP BY trips.id
                        ORDER BY min(date_start) DESC
                        LIMIT ?
                        OFFSET ?
                    |] pageSize (page * pageSize)
                Identity num_trips <- H.singleEx [H.stmt|
                        SELECT count(*) FROM trips
                    |]
                return (trips, num_trips)
        case dbres of
            Left err -> error $ show err -- TODO
            Right (trips,num_trips) ->
                    defaultLayout $ do
                        let pages = filter (>= 0) $
                                      filter (< num_trips `div` pageSize + 1)
                                      [(page - 2)..(page + 2)]
                        setTitle "Brandreth Guestbook"
                        $(widgetFile "trips")
