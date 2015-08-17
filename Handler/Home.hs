{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.Home where

import Import
import Utils.Database
import Utils.Days
import Utils.Users
import Data.Time

import qualified Hasql as H
import qualified Data.Text as T

getHomeR :: Handler Html
getHomeR = do
    req <- waiRequest
    checkIfAllowed req
    admin <- liftIO $ isAdmin req
    let muser = getUser req
    case muser of
        Just user -> do
            dbres <- liftIO $ do
                conn <- getDbConn
                H.session conn $ H.tx Nothing $ do
                    ((pid,name,mnick) :: (Int,T.Text,Maybe T.Text))
                        <- H.singleEx $ [H.stmt|
                            SELECT people.id
                                 , people.name
                                 , people.nickname
                            FROM people
                            WHERE people.csh_username = ?
                        |] user
                    (lastEntry :: Maybe (Int,Day,Day,T.Text))
                        <- H.maybeEx $ [H.stmt|
                            SELECT entries.trip_id
                                 , entries.date_start
                                 , entries.date_end
                                 , entries.entry
                            FROM entries
                            INNER JOIN trips ON (entries.trip_id = trips.id)
                            WHERE entries.person_id = ?
                            ORDER BY date_start DESC
                            LIMIT 1
                        |] pid
                    return ((pid,name,mnick),lastEntry)
            case dbres of
                Left err -> error $ show err
                Right ((pid,name,mnick),lastEntry) -> do
                    today <- liftIO $ getCurrentTime >>= return . utctDay
                    defaultLayout $ do
                        setTitle $ "Brandreth Guestbook"
                        $(widgetFile "homepage-user")
        Nothing ->
            defaultLayout $ do
                setTitle "Brandreth Guestbook"
                $(widgetFile "homepage")
