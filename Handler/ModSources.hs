{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.ModSources where

import Import
import Utils.Database
import Yesod.Form.Bootstrap3

import qualified Hasql as H
import qualified Data.Text as T

getModSourcesR :: Handler Html
getModSourcesR = do
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (sources :: [(Int,T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT *
                    FROM sources
                    ORDER BY source ASC
                |]
            return sources
    case dbres of
        Left err -> error $ show err
        Right sources -> do
            (widget, enctype) <- generateFormPost newSourceForm
            defaultLayout $ do
                setTitle $ "Sources | Brandreth Guestbook"
                $(widgetFile "modsources")

data PSource = PSource T.Text

newSourceAForm :: AForm Handler PSource
newSourceAForm = PSource <$> areq textField "New Source" Nothing
                        <* bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

newSourceForm :: Html -> MForm Handler (FormResult PSource, Widget)
newSourceForm = renderBootstrap3 BootstrapBasicForm newSourceAForm

postModSourcesR :: Handler Html
postModSourcesR = do
    ((result, _), _) <- runFormPost newSourceForm
    case result of
        FormSuccess (PSource s) -> do
            dbres <- liftIO $ do
                conn <- getDbConn
                H.session conn $ H.tx Nothing $
                    H.unitEx $ [H.stmt|
                            INSERT INTO "sources" (source)
                            VALUES (?)
                        |] s
            case dbres of
                Left err -> error $ show err
                Right _ -> getModSourcesR
        FormMissing -> error $ "No form data sent!"
        FormFailure err -> error $ show err

getDelSourceR :: Int -> Handler Html
getDelSourceR rid = do
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            Identity (num_trips :: Int) <- H.singleEx $ [H.stmt|
                    SELECT count(*)
                    FROM people
                    WHERE people.source = ?
                |] rid
            if num_trips /= 0
                then return False
                else do
                    H.unitEx $ [H.stmt|
                            DELETE FROM sources
                            WHERE id = ?
                        |] rid
                    return True
    case dbres of
        Left err -> error $ show err
        Right False -> defaultLayout $
            [whamlet|
                <h3>There are still people using that source!
            |]
        Right True -> getModSourcesR
