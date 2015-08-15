{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.EditEntry where

import Import
import Utils.Database
import Utils.Users
import Yesod.Form.Bootstrap3

import qualified Hasql as H
import qualified Data.Text as T

getEditEntryR :: Int -> Handler Html
getEditEntryR entryId = do
    req <- waiRequest
    restrictToAdmins req
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
            (entry :: (Int,Int,Day,Day,T.Text)) <- H.singleEx $ [H.stmt|
                    SELECT person_id
                         , trip_id
                         , date_start
                         , date_end
                         , entry
                    FROM entries
                    WHERE id = ?
                |] entryId
            return (people,entry)
    case dbres of
        Left err -> error $ show err
        Right (people,(pid,tid,st,en,entry)) -> do
            (widget, enctype) <- generateFormPost $ editEntryForm
                                                        (applyNicks people)
                                                        (pid,st,en,entry)
            defaultLayout $ do
                setTitle $ "Edit Entry | Brandreth Guestbook"
                $(widgetFile "editentry")

data Entry = Entry Int Day Day Textarea

editEntryAForm :: [(Int,T.Text)] -> (Int,Day,Day,T.Text) ->AForm Handler Entry
editEntryAForm people (pid,start,end,entry) =
        Entry <$> areq (selectFieldList speople) "Person" (Just pid)
              <*> areq dayField "Day Arrived" (Just start)
              <*> areq dayField "Day Left" (Just end)
              <*> areq textareaField "Entry" (Just $ Textarea entry)
              <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)
    where speople = map (\(x,y) -> (y,x)) people

editEntryForm :: [(Int,T.Text)]
              -> (Int,Day,Day,T.Text)
              -> Html
              -> MForm Handler (FormResult Entry, Widget)
editEntryForm people entry = renderBootstrap3
                (BootstrapHorizontalForm (ColSm 0)(ColSm 2)(ColSm 0)(ColSm 10))
                $ editEntryAForm people entry

applyNicks :: [(Int,T.Text,Maybe T.Text)] -> [(Int,T.Text)]
applyNicks []                            = []
applyNicks ((pid,name,Nothing):people)   = (pid,name) : applyNicks people
applyNicks ((pid,name,Just nick):people) = (pid,name `T.append` " - "
                                                     `T.append` nick)
                                                        : applyNicks people

postEditEntryR :: Int -> Handler Html
postEditEntryR entryId = do
    req <- waiRequest
    restrictToAdmins req
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
            (entry :: (Int,Int,Day,Day,T.Text)) <- H.singleEx $ [H.stmt|
                    SELECT person_id
                         , trip_id
                         , date_start
                         , date_end
                         , entry
                    FROM entries
                    WHERE id = ?
                |] entryId
            return (conn, people,entry)
    case dbres of
        Left err -> error $ show err
        Right (conn, people,(pid,_,st,en,entry)) -> do
            ((result, _), _) <- runFormPost (editEntryForm (applyNicks people)
                                                    (pid,st,en,entry))
            case result of
                FormSuccess (Entry pid' st' en' entry') -> do
                    dbres' <- liftIO $ do
                        H.session conn $ H.tx Nothing $ do
                            H.singleEx $ [H.stmt|
                                    UPDATE "entries"
                                    SET person_id = ?
                                      , date_start = ?
                                      , date_end = ?
                                      , entry = ?
                                    WHERE id = ?
                                    RETURNING trip_id
                                |] pid' st' en' (unTextarea entry') entryId
                    case dbres' of
                        Left err -> error $ show err
                        Right (Identity tripId) -> redirect $ TripR tripId
                FormMissing -> error $ "No form data sent!"
                FormFailure err -> error $ show err
