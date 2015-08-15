{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.EditPerson where

import Import
import Utils.Database
import Utils.Users
import Yesod.Form.Bootstrap3

import qualified Hasql as H
import qualified Data.Text as T

getEditPersonR :: Int -> Handler Html
getEditPersonR personId = do
    req <- waiRequest
    restrictToAdmins req
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (sources :: [(Int,T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT *
                    FROM sources
                    ORDER BY source ASC
                |]
            (person :: (T.Text,Maybe T.Text,Maybe T.Text,Int,Bool))
                <- H.singleEx $ [H.stmt|
                    SELECT name
                         , nickname
                         , csh_username
                         , source
                         , admin
                    FROM people
                    WHERE id = ?
                |] personId
            return (sources,person)
    case dbres of
        Left err -> error $ show err
        Right (sources,person) -> do
            (widget, enctype) <- generateFormPost
                                    $ editPersonForm sources person
            defaultLayout $ do
                setTitle $ "Edit Person | Brandreth Guestbook"
                $(widgetFile "editperson")

data Person = Person T.Text (Maybe T.Text) (Maybe T.Text) Int Bool

editPersonAForm :: [(Int,T.Text)]
                -> (T.Text,Maybe T.Text,Maybe T.Text,Int,Bool)
                -> AForm Handler Person
editPersonAForm sources (na,ni,us,so,ad) =
        Person <$> areq textField "Name" (Just na)
               <*> aopt textField "Nickname" (Just ni)
               <*> aopt textField "CSH Username" (Just us)
               <*> areq (selectFieldList ssources) "Source" (Just so)
               <*> areq boolField "Admin" (Just ad)
               <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)
    where ssources = map (\(x,y) -> (y,x)) sources

editPersonForm :: [(Int,T.Text)]
                -> (T.Text,Maybe T.Text,Maybe T.Text,Int,Bool)
              -> Html
              -> MForm Handler (FormResult Person, Widget)
editPersonForm sources person = renderBootstrap3
                (BootstrapHorizontalForm (ColSm 0)(ColSm 2)(ColSm 0)(ColSm 10))
                $ editPersonAForm sources person

postEditPersonR :: Int -> Handler Html
postEditPersonR pid = do
    req <- waiRequest
    restrictToAdmins req
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (sources :: [(Int,T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT *
                    FROM sources
                    ORDER BY source ASC
                |]
            (person :: (T.Text,Maybe T.Text,Maybe T.Text,Int,Bool))
                <- H.singleEx $ [H.stmt|
                    SELECT name
                         , nickname
                         , csh_username
                         , source
                         , admin
                    FROM people
                    WHERE id = ?
                |] pid
            return (conn, sources, person)
    case dbres of
        Left err -> error $ show err
        Right (conn, sources, person) -> do
            ((result, _), _) <- runFormPost (editPersonForm sources person)
            case result of
                FormSuccess (Person na ni us so ad) -> do
                    dbres' <- liftIO $ do
                        H.session conn $ H.tx Nothing $ do
                            H.unitEx $ [H.stmt|
                                    UPDATE "people"
                                    SET name = ?
                                      , nickname = ?
                                      , csh_username = ?
                                      , source = ?
                                      , admin = ?
                                    WHERE id = ?
                                |] na ni us so ad pid
                    case dbres' of
                        Left err -> error $ show err
                        Right _ -> redirect $ PersonR pid
                FormMissing -> error $ "No form data sent!"
                FormFailure err -> error $ show err
