{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.NewTrip where

import Import
import Utils.Database
import Utils.Users
import Yesod.Form.Bootstrap3

import qualified Hasql as H
import qualified Data.Text as T

getNewTripR :: Handler Html
getNewTripR = do
    req <- waiRequest
    restrictToAdmins req
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (reasons :: [(Int,T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT *
                    FROM reasons
                    ORDER BY reason ASC
                |]
            return reasons
    case dbres of
        Left err -> error $ show err
        Right reasons -> do
            (widget, enctype) <- generateFormPost $ newTripForm reasons
            defaultLayout $ do
                setTitle $ "New Trip | Brandreth Guestbook"
                $(widgetFile "newtrip")

data TripReason = TripReason Int

newTripAForm :: [(Int,T.Text)] -> AForm Handler TripReason
newTripAForm reasons =
        TripReason <$> areq (selectFieldList sreasons) "Trip Reason" Nothing
                   <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)
    where sreasons = map (\(x,y) -> (y,x)) reasons

newTripForm :: [(Int,T.Text)]
            -> Html
            -> MForm Handler (FormResult TripReason, Widget)
newTripForm reasons = renderBootstrap3 BootstrapBasicForm $ newTripAForm reasons

postNewTripR :: Handler Html
postNewTripR = do
    req <- waiRequest
    restrictToAdmins req
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (reasons :: [(Int,T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT *
                    FROM reasons
                    ORDER BY reason ASC
                |]
            return (conn, reasons)
    case dbres of
        Left err -> error $ show err
        Right (conn, reasons) -> do
            ((result, _), _) <- runFormPost (newTripForm reasons)
            case result of
                FormSuccess (TripReason r) -> do
                    dbres' <- liftIO $ do
                        H.session conn $ H.tx Nothing $ do
                            Identity tid <- H.singleEx $ [H.stmt|
                                    INSERT INTO "trips" (reason_id)
                                    VALUES (?)
                                    RETURNING id
                                |] r
                            return tid
                    case dbres' of
                        Left err -> error $ show err
                        Right tid -> redirect $ TripR tid
                FormMissing -> error $ "No form data sent!"
                FormFailure err -> error $ show err
