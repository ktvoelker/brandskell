{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.EditTrip where

import Import
import Utils.Database
import Yesod.Form.Bootstrap3

import qualified Hasql as H
import qualified Data.Text as T

getEditTripR :: Int -> Handler Html
getEditTripR tripId = do
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (reasons :: [(Int,T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT *
                    FROM reasons
                    ORDER BY reason ASC
                |]
            (Identity (reason :: Int)) <- H.singleEx $ [H.stmt|
                    SELECT reason_id
                    FROM trips
                    WHERE id = ?
                |] tripId
            return (reasons,reason)
    case dbres of
        Left err -> error $ show err
        Right (reasons,reason) -> do
            (widget, enctype) <- generateFormPost $ editTripForm reasons reason
            defaultLayout $ do
                setTitle $ "Edit Trip | Brandreth Guestbook"
                $(widgetFile "edittrip")

data TripReason = TripReason Int

editTripAForm :: [(Int,T.Text)] -> Int -> AForm Handler TripReason
editTripAForm reasons reason =
        TripReason <$> areq (selectFieldList sreasons) "Trip Reason"
                                                            (Just reason)
                   <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)
    where sreasons = map (\(x,y) -> (y,x)) reasons

editTripForm :: [(Int,T.Text)]
             -> Int
             -> Html
             -> MForm Handler (FormResult TripReason, Widget)
editTripForm reasons reason =
            renderBootstrap3 BootstrapBasicForm $ editTripAForm reasons reason

postEditTripR :: Int -> Handler Html
postEditTripR tripId = do
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (reasons :: [(Int,T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT *
                    FROM reasons
                    ORDER BY reason ASC
                |]
            (Identity (reason :: Int)) <- H.singleEx $ [H.stmt|
                    SELECT reason_id
                    FROM trips
                    WHERE id = ?
                |] tripId
            return (conn, reasons, reason)
    case dbres of
        Left err -> error $ show err
        Right (conn, reasons, reason) -> do
            ((result, _), _) <- runFormPost (editTripForm reasons reason)
            case result of
                FormSuccess (TripReason r) -> do
                    dbres' <- liftIO $ do
                        H.session conn $ H.tx Nothing $ do
                            H.unitEx $ [H.stmt|
                                    UPDATE "trips"
                                    SET reason_id = ?
                                    WHERE id = ?
                                |] r tripId
                    case dbres' of
                        Left err -> error $ show err
                        Right _ -> redirect $ TripR tripId
                FormMissing -> error $ "No form data sent!"
                FormFailure err -> error $ show err
