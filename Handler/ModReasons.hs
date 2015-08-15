{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.ModReasons where

import Import
import Utils.Database
import Utils.Users
import Yesod.Form.Bootstrap3

import qualified Hasql as H
import qualified Data.Text as T

getModReasonsR :: Handler Html
getModReasonsR = do
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
            (widget, enctype) <- generateFormPost newReasonForm
            defaultLayout $ do
                setTitle $ "Reasons | Brandreth Guestbook"
                $(widgetFile "modreasons")

data Reason = Reason T.Text

newReasonAForm :: AForm Handler Reason
newReasonAForm = Reason <$> areq textField "New Reason" Nothing
                        <* bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

newReasonForm :: Html -> MForm Handler (FormResult Reason, Widget)
newReasonForm = renderBootstrap3 BootstrapBasicForm newReasonAForm

postModReasonsR :: Handler Html
postModReasonsR = do
    req <- waiRequest
    restrictToAdmins req
    ((result, _), _) <- runFormPost newReasonForm
    case result of
        FormSuccess (Reason r) -> do
            dbres <- liftIO $ do
                conn <- getDbConn
                H.session conn $ H.tx Nothing $
                    H.unitEx $ [H.stmt|
                            INSERT INTO "reasons" (reason)
                            VALUES (?)
                        |] r
            case dbres of
                Left err -> error $ show err
                Right _ -> redirect ModReasonsR
        FormMissing -> error $ "No form data sent!"
        FormFailure err -> error $ show err

getDelReasonR :: Int -> Handler Html
getDelReasonR rid = do
    req <- waiRequest
    restrictToAdmins req
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            Identity (num_trips :: Int) <- H.singleEx $ [H.stmt|
                    SELECT count(*)
                    FROM trips
                    WHERE trips.reason_id = ?
                |] rid
            if num_trips /= 0
                then return False
                else do
                    H.unitEx $ [H.stmt|
                            DELETE FROM reasons
                            WHERE id = ?
                        |] rid
                    return True
    case dbres of
        Left err -> error $ show err
        Right False -> defaultLayout $
            [whamlet|
                <h3>There are still trips using that reason!
            |]
        Right True -> redirect ModReasonsR
