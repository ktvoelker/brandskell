{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.Search where

import Import
import Utils.Database
import Utils.Days
import Utils.Users
import Data.Char

import qualified Hasql as H
import qualified Data.Text as T

getSearchR :: T.Text -> Handler Html
getSearchR query = do
    req <- waiRequest
    checkIfAllowed req
    dbres <- liftIO $ do
        let sanitizedQuery = foldr (\w a -> if a /= ""
                                            then a `T.append` " & " `T.append` w
                                            else w
                                ) "" (T.words $ T.filter (isAlphaNum) query)
        print sanitizedQuery
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (results :: [(Int,Int,T.Text,Maybe T.Text,T.Text,Day,Day,T.Text)])
                        <- H.listEx $ [H.stmt|
                    SELECT tid
                         , pid
                         , name
                         , nickname
                         , reason
                         , date_start
                         , date_end
                         , entry
                    FROM (
                        SELECT trips.id as tid
                             , people.id as pid
                             , people.name
                             , people.nickname
                             , reason
                             , date_start
                             , date_end
                             , entry
                             , to_tsvector(entry) || to_tsvector(name)
                                           AS document
                        FROM entries
                        INNER JOIN people ON (entries.person_id = people.id)
                        INNER JOIN trips ON (entries.trip_id = trips.id)
                        INNER JOIN reasons ON (trips.reason_id = reasons.id)
                    ) p_search
                    WHERE p_search.document @@ to_tsquery(?)
                    ORDER BY date_start DESC
                |] sanitizedQuery
            return results
    case dbres of
        Left err -> error $ show err
        Right results ->
            defaultLayout $ do
                setTitle $ "Search Results | Brandreth Guestbook"
                $(widgetFile "search")
