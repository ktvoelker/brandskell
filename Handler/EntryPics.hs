{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}
module Handler.EntryPics where

import Import
import Utils.Database

import qualified Hasql as H
import qualified Data.Text as T

getEntryPicsR :: Int -> Handler Html
getEntryPicsR entryId = do
    dbres <- liftIO $ do
        conn <- getDbConn
        H.session conn $ H.tx Nothing $ do
            (images :: [(Int,T.Text)]) <- H.listEx $ [H.stmt|
                    SELECT id
                         , ext
                    FROM images
                    WHERE entry_id = ? AND img_type = 'entry'
                    ORDER BY id ASC
                |] entryId
            return images
    case dbres of
        Left err -> error $ show err
        Right images ->
            defaultLayout $ do
                setTitle $ "Entry Images | Brandreth Guestbook"
                $(widgetFile "entrypics")
