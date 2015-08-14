{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.NewPerson where

import Import
import Utils.Database
import Yesod.Form.Bootstrap3

import qualified Hasql as H
import qualified Data.Text as T

getNewPersonR :: Int -> Handler Html
getNewPersonR tripId = do
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
            (widget, enctype) <- generateFormPost $ newPersonForm sources
            defaultLayout $ do
                setTitle $ "New Person | Brandreth Guestbook"
                $(widgetFile "newperson")

data Person = Person T.Text T.Text T.Text Int

newPersonAForm :: [(Int,T.Text)] -> AForm Handler Person
newPersonAForm sources =
        Person <$> areq textField "Name" Nothing
               <*> areq textField "Nickname" Nothing
               <*> areq textField "CSH Username" Nothing
               <*> areq (selectFieldList ssources) "Source" Nothing
               <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)
    where ssources = map (\(x,y) -> (y,x)) sources

newPersonForm :: [(Int,T.Text)]
              -> Html
              -> MForm Handler (FormResult Person, Widget)
newPersonForm sources = renderBootstrap3
                (BootstrapHorizontalForm (ColSm 0)(ColSm 2)(ColSm 0)(ColSm 10))
                $ newPersonAForm sources

toM :: T.Text -> Maybe T.Text
toM "" = Nothing
toM t = Just t

postNewPersonR :: Int -> Handler Html
postNewPersonR tripId = do
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (sources :: [(Int,T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT *
                    FROM sources
                    ORDER BY source ASC
                |]
            return (conn, sources)
    case dbres of
        Left err -> error $ show err
        Right (conn, sources) -> do
            ((result, _), _) <- runFormPost (newPersonForm sources)
            case result of
                FormSuccess (Person na ni us so) -> do
                    dbres' <- liftIO $ do
                        H.session conn $ H.tx Nothing $ do
                            H.unitEx $ [H.stmt|
                                    INSERT INTO "people"
                                        ( name
                                        , nickname
                                        , csh_username
                                        , source
                                        , admin
                                        )
                                    VALUES (?,?,?,?,false)
                                |] na (toM ni) (toM us) so
                    case dbres' of
                        Left err -> error $ show err
                        Right _ -> redirect $ NewEntryR tripId
                FormMissing -> error $ "No form data sent!"
                FormFailure err -> error $ show err
