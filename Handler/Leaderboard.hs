{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.Leaderboard where

import Import
import Utils.Database
import Utils.Users

import qualified Hasql as H
import qualified Data.Text as T

assignRank :: [(a,b,c,Int)] -> [(a,b,c,Int,Int)]
assignRank ppl = assignRankHelper 1 ppl

assignRankHelper :: Int -> [(a,b,c,Int)] -> [(a,b,c,Int,Int)]
assignRankHelper _ [] = []
assignRankHelper rank ((a,b,c,n):(a',b',c',n'):xs)
    | n == n'   = (a,b,c,n,rank):assignRankHelper rank ((a',b',c',n'):xs)
    | otherwise = (a,b,c,n,rank):assignRankHelper (rank + 1) ((a',b',c',n'):xs)
assignRankHelper rank ((a,b,c,n):xs) =
            (a,b,c,n,rank):assignRankHelper (rank + 1) xs

getLeaderboardTripsR :: Handler Html
getLeaderboardTripsR = do
    req <- waiRequest
    checkIfAllowed req
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (people :: [(Int,T.Text,Maybe T.Text,Int)]) <- H.listEx $ [H.stmt|
                    SELECT people.id
                         , people.name
                         , people.nickname
                         , count(entries.*)
                    FROM people
                    INNER JOIN entries ON (people.id = entries.person_id)
                    GROUP BY people.id
                    ORDER BY count DESC, people.name ASC
                |]
            return (assignRank people)
    case dbres of
        Left err -> error $ show err
        Right people ->
            defaultLayout $ do
                setTitle "Leaderboard by Trip Count | Brandreth Guestbook"
                $(widgetFile "leaderboardtrips")

getLeaderboardDurationR :: Handler Html
getLeaderboardDurationR = do
    req <- waiRequest
    checkIfAllowed req
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (people :: [(Int,T.Text,Maybe T.Text,Int)]) <- H.listEx $ [H.stmt|
                    SELECT people.id
                         , people.name
                         , people.nickname
                         , sum(entries.date_end - entries.date_start)
                    FROM people
                    INNER JOIN entries ON (people.id = entries.person_id)
                    GROUP BY people.id
                    ORDER BY sum DESC, people.name ASC
                |]
            return (assignRank people)
    case dbres of
        Left err -> error $ show err
        Right people ->
            defaultLayout $ do
                setTitle "Leaderboard by Trip Duration | Brandreth Guestbook"
                $(widgetFile "leaderboardduration")
