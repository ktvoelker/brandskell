{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.Stats where

import Import
import Utils.Database

import qualified Hasql as H
import qualified Data.List as L

mapInsert :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
mapInsert [] _ _ = []
mapInsert ((y,x):lst) y' x'
    | y == y' = (y',x'):lst
    | otherwise = (y,x) : mapInsert lst y' x'

collapseLists :: (Eq a,Ord a,Enum b,Num b) => [[(a,b)]] -> ([a],[[b]])
collapseLists dat = (years,vals')
    where years = L.sort $ L.nub $ concatMap (map fst) dat
          ydat = zip years (repeat 0)
          vals = map (foldr (\(y,x) years' -> mapInsert years' y x) ydat) dat
          vals' = map (map snd) vals

getStatsR :: Handler Html
getStatsR = do
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (trips :: [(Double,Int)]) <- H.listEx $ [H.stmt|
                    SELECT date_part('year', date_start)
                         , count(*)
                    FROM entries
                    GROUP BY date_part
                    ORDER BY date_part
                |]
            (uniqTrips :: [(Double,Int)]) <- H.listEx $ [H.stmt|
                    SELECT date_part('year', date_start)
                         , count(DISTINCT person_id)
                    FROM entries
                    GROUP BY date_part
                    ORDER BY date_part
                |]
            (noobTrips :: [(Double,Int)]) <- H.listEx $ [H.stmt|
                    SELECT date_part
                         , count(person_id)
                    FROM (
                        SELECT person_id
                             , date_part('year', min(date_start))
                        FROM entries
                        GROUP BY person_id
                    ) AS q
                    GROUP BY date_part
                    ORDER BY date_part
                |]
            (days :: [(Double,Int)]) <- H.listEx $ [H.stmt|
                    SELECT date_part('year', date_start)
                         , sum(date_end - date_start)
                    FROM entries
                    GROUP BY date_part
                    ORDER BY date_part
                |]
            return $ collapseLists [trips,uniqTrips,noobTrips,days]
    case dbres of
        Left err -> error $ show err
        Right (years,[trips,uniqTrips,noobTrips,days]) ->
            defaultLayout $ do
                setTitle $ "Stats | Brandreth Guestbook"
                $(widgetFile "stats")
        -- Should never happen
        Right _ -> error $ "Something broke. You should tell someone."
