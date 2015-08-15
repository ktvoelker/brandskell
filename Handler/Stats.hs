{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.Stats where

import Import
import Utils.Database
import Utils.Users

import qualified Hasql as H
import qualified Data.List as L
import qualified Data.Text as T

collapseLists :: (Eq a,Ord a,Enum b,Num b) => [[(a,b)]] -> ([a],[[b]])
collapseLists dat = (years,vals')
    where years = L.sort $ L.nub $ concatMap (map fst) dat
          ydat  = zip years (repeat 0)
          vals  = map (foldr (\(y,x) years' -> mapInsert years' y x) ydat) dat
          vals' = map (map snd) vals

mapInsert :: Eq a => [(a,b)] -> a -> b -> [(a,b)]
mapInsert [] _ _ = []
mapInsert ((y,x):lst) y' x'
    | y == y'   = (y',x') : lst
    | otherwise = (y,x) : mapInsert lst y' x'

collapseList2 :: (Eq a,Eq b,Ord a,Ord b) => [(a,b)] -> ([a],[(b,[Int])])
collapseList2 yss = (years,sCounts)
    where sources = L.sort $ L.nub $ map snd yss
          years   = L.sort $ L.nub $ map fst yss
          sZeros  = zip sources (repeat $ map (\_ -> 0) years)
          sCounts = foldr (incSeries years) sZeros yss

incSeries :: (Eq a,Eq b) => [a] -> (a,b) -> [(b,[Int])] -> [(b,[Int])]
incSeries _ _ [] = [] -- should never happen
incSeries reasons (r,m) ((month,counts):mcs)
    | month == m = ((month,inc (listIndex reasons r) counts) : mcs)
    | otherwise  = (month,counts) : incSeries reasons (r,m) mcs

listIndex :: Eq a => [a] -> a -> Int
listIndex [] _ = 0 -- This should never happen
listIndex (t:ts) t'
    | t == t'   = 0
    | otherwise = 1 + listIndex ts t'

inc :: Int -> [Int] -> [Int]
inc _ []     = [] -- this should never happen
inc 0 (x:xs) = x + 1 : xs
inc n (x:xs) = x : inc (n - 1) xs

showMonth :: Int -> T.Text
showMonth 1  = "January"
showMonth 2  = "February"
showMonth 3  = "March"
showMonth 4  = "April"
showMonth 5  = "May"
showMonth 6  = "June"
showMonth 7  = "July"
showMonth 8  = "August"
showMonth 9  = "September"
showMonth 10 = "October"
showMonth 11 = "November"
showMonth 12 = "December"
showMonth _  = "Corn Subsidies"

data SourceYears = SourceYears T.Text [Int]
instance ToJSON SourceYears where
        toJSON (SourceYears src yrs) =
            object [ "name" .= src
                   , "data" .= yrs
                   ]
toSY :: (T.Text,[Int]) -> SourceYears
toSY (t,y) = SourceYears t y

getStatsR :: Handler Html
getStatsR = do
    req <- waiRequest
    checkIfAllowed req
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
            (monthlyReasons :: [(T.Text,Double)]) <- H.listEx $ [H.stmt|
                    SELECT min(reason)
                         , date_part('month', min(date_start))
                    FROM entries
                    INNER JOIN trips ON (entries.trip_id = trips.id)
                    INNER JOIN reasons ON (trips.reason_id = reasons.id)
                    GROUP BY trips
                |]
            (yearlySources :: [(Double,T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT date_part('year', date_start)
                         , sources.source
                    FROM entries
                    INNER JOIN people ON (entries.person_id = people.id)
                    INNER JOIN sources ON (people.source = sources.id)
                |]
            let (reasons,monthCounts) = collapseList2
                            $ map (\(r,m) -> (r,round m :: Int)) monthlyReasons
            return ( collapseLists [trips,uniqTrips,noobTrips,days]
                   , (reasons, map (\(m,cs) -> (showMonth m,cs)) monthCounts)
                   , collapseList2 (map (\(y,s) -> (round y :: Int,s))
                                                yearlySources)
                   )
    case dbres of
        Left err -> error $ show err
        Right ( (years,[trips,uniqTrips,noobTrips,days])
              , (reasons,monthCounts)
              , (years1,yearlySources)
              ) ->
            defaultLayout $ do
                setTitle $ "Stats | Brandreth Guestbook"
                $(widgetFile "stats")
        -- Should never happen
        Right _ -> error $ "Something broke. You should tell someone."
