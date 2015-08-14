{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.NewEntry where

import Import
import Utils.Database
import Yesod.Form.Bootstrap3

import qualified Hasql as H
import qualified Data.Text as T

getNewEntryR :: Int -> Handler Html
getNewEntryR tripId = do
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (people :: [(Int,T.Text,Maybe T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT id
                         , name
                         , nickname
                    FROM people
                    ORDER BY name ASC
                |]
            return people
    case dbres of
        Left err -> error $ show err
        Right people -> do
            (widget, enctype) <- generateFormPost
                                    $ newEntryForm (applyNicks people)
            defaultLayout $ do
                setTitle $ "New Entry | Brandreth Guestbook"
                $(widgetFile "newentry")

data Entry = Entry Int Day Day Textarea

newEntryAForm :: [(Int,T.Text)] -> AForm Handler Entry
newEntryAForm people =
        Entry <$> areq (selectFieldList speople) "Person" Nothing
              <*> areq dayField "Day Arrived" Nothing
              <*> areq dayField "Day Left" Nothing
              <*> areq textareaField "Entry" Nothing
              <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)
    where speople = map (\(x,y) -> (y,x)) people

newEntryForm :: [(Int,T.Text)]
             -> Html
             -> MForm Handler (FormResult Entry, Widget)
newEntryForm people = renderBootstrap3
                (BootstrapHorizontalForm (ColSm 0)(ColSm 2)(ColSm 0)(ColSm 10))
                $ newEntryAForm people

applyNicks :: [(Int,T.Text,Maybe T.Text)] -> [(Int,T.Text)]
applyNicks []                            = []
applyNicks ((pid,name,Nothing):people)   = (pid,name) : applyNicks people
applyNicks ((pid,name,Just nick):people) = (pid,name `T.append` " - "
                                                     `T.append` nick)
                                                        : applyNicks people

postNewEntryR :: Int -> Handler Html
postNewEntryR tripId = do
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (people :: [(Int,T.Text,Maybe T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT id
                         , name
                         , nickname
                    FROM people
                    ORDER BY name ASC
                |]
            return (conn, people)
    case dbres of
        Left err -> error $ show err
        Right (conn, people) -> do
            ((result, _), _) <- runFormPost (newEntryForm (applyNicks people))
            case result of
                FormSuccess (Entry pid st en entry) -> do
                    dbres' <- liftIO $ do
                        H.session conn $ H.tx Nothing $ do
                            H.unitEx $ [H.stmt|
                                    INSERT INTO "entries"
                                        ( person_id
                                        , trip_id
                                        , date_start
                                        , date_end
                                        , entry
                                        )
                                    VALUES (?,?,?,?,?)
                                |] pid tripId st en (unTextarea entry)
                    case dbres' of
                        Left err -> error $ show err
                        Right _ -> redirect $ TripR tripId
                FormMissing -> error $ "No form data sent!"
                FormFailure err -> error $ show err
