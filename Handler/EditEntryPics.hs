{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.EditEntryPics where

import Import
import Utils.Database
import Utils.Users
import Yesod.Form.Bootstrap3

import qualified Hasql as H
import qualified Data.Text as T

imgUrl :: String
imgUrl = "/static/images/"

imgPath :: String
imgPath = "." ++ imgUrl

lastElem :: [a] -> a
lastElem []     = error "lastElem called on empty list"
lastElem [x]    = x
lastElem (_:xs) = lastElem xs

getEditEntryPicsR :: Int -> Int -> Handler Html
getEditEntryPicsR tripId entryId = do
    req <- waiRequest
    restrictToAdmins req
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (images :: [(Int,T.Text,T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT id
                         , img_type
                         , ext
                    FROM images
                    WHERE entry_id = ?
                    ORDER BY id ASC
                |] entryId
            return images
    case dbres of
        Left err -> error $ show err
        Right images -> do
            let entries = filter ((== "entry") . (\(_,x,_) -> x)) images
                doodles = filter ((== "doodle") . (\(_,x,_) -> x)) images
            (widget, enctype) <- generateFormPost $ editEntryPicsForm
            (widget', enctype') <- generateFormPost $ editEntryDoodleForm
            defaultLayout $ do
                setTitle $ "Edit Entry Pictures | Brandreth Guestbook"
                $(widgetFile "editentrypics")

data EntryPic = EntryPic FileInfo

editEntryPicsAForm :: AForm Handler EntryPic
editEntryPicsAForm =
        EntryPic <$> fileAFormReq "Upload a new guestbook picture"
                 <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

editEntryPicsForm ::  Html
                  -> MForm Handler (FormResult EntryPic, Widget)
editEntryPicsForm = renderBootstrap3
                (BootstrapHorizontalForm (ColSm 0)(ColSm 2)(ColSm 0)(ColSm 10))
                $ editEntryPicsAForm

editEntryDoodleAForm :: AForm Handler EntryPic
editEntryDoodleAForm =
        EntryPic <$> fileAFormReq "Upload a new doodle"
                 <*  bootstrapSubmit ("Submit" :: BootstrapSubmit Text)

editEntryDoodleForm ::  Html -> MForm Handler (FormResult EntryPic, Widget)
editEntryDoodleForm = renderBootstrap3
                (BootstrapHorizontalForm (ColSm 0)(ColSm 2)(ColSm 0)(ColSm 10))
                $ editEntryPicsAForm 

postEditEntryPicsR :: Int -> Int -> Handler Html
postEditEntryPicsR = postNewPicture "entry"

postEditEntryDoodlesR :: Int -> Int -> Handler Html
postEditEntryDoodlesR = postNewPicture "doodle"

postNewPicture :: T.Text -> Int -> Int -> Handler Html
postNewPicture imgType tripId entryId = do
    req <- waiRequest
    restrictToAdmins req

    ((result, _), _) <- runFormPost editEntryPicsForm
    case result of
        FormSuccess (EntryPic fi) -> do
            let fnametoks = T.splitOn "." $ fileName fi
                ext = if length fnametoks > 0
                          then lastElem fnametoks
                          else "jpg"
            dbres <- liftIO $ do
                conn <- getDbConn
                H.session conn $ H.tx Nothing $ do
                    H.singleEx $ [H.stmt|
                            INSERT INTO "images"
                                (entry_id,img_type,ext)
                            VALUES (?,?,?)
                            RETURNING id
                        |] entryId imgType ext
            case dbres of
                Left err -> error $ show err
                Right (Identity (imgId :: Int)) -> do
                    let filename = imgPath ++ (show imgId)
                                    ++ "." ++ (T.unpack ext)
                    liftIO $ fileMove fi filename
                    redirect $ EditEntryPicsR tripId entryId
        FormMissing -> error $ "No form data sent!"
        FormFailure err -> error $ show err

getDelEntryPicR :: Int -> Int -> Int -> Handler Html
getDelEntryPicR tripId entryId imgId = do
    req <- waiRequest
    restrictToAdmins req
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            H.unitEx $ [H.stmt|
                    DELETE FROM images
                    WHERE id = ?
                |] imgId
    case dbres of
        Left err -> error $ show err
        Right _ -> redirect $ EditEntryPicsR tripId entryId
