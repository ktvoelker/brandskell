module Utils.Database where

import Data.Maybe
import Data.Time.Calendar

import qualified Hasql as H
import qualified Hasql.Postgres as HP
import qualified Data.Text as T

data Reason = Reason { reasonId   :: Int
                     , reasonText :: T.Text
                     } deriving(Show,Eq)

data Trip = Trip { tripId       :: Int
                 , tripReasonId :: Int
                 } deriving(Show,Eq)

data Source = Source { sourceId   :: Int
                     , sourceText :: T.Text
                     } deriving(Show,Eq)

data Person = Person { personId       :: Int
                     , personName     :: T.Text
                     , personNick     :: Maybe T.Text
                     , personUsername :: Maybe T.Text
                     , personSourceId :: Int
                     } deriving(Show,Eq)

data Entry = Entry { entryPersonId :: Int
                   , entryTripId   :: Int
                   , entryStart    :: Day
                   , entryEnd      :: Day
                   , entryEntry    :: T.Text
                   , entryImage    :: Maybe T.Text
                   } deriving(Show,Eq)

poolMVar :: MVar (H.Pool HP.Postgres)
poolMVar = newEmptyMVar

postgresSettings :: HP.Settings
postgresSettings = HP.ParamSettings "localhost"      -- Host
                                    5432             -- Port
                                    "brandreth"      -- User
                                    "treestheyareus" -- Password
                                    "postgres"       -- Database

-- fromJust is used because the following will never be Nothing.
-- 6 and 30 both fall within valid bounds.
poolSettings :: H.PoolSettings
poolSettings = fromJust $ H.poolSettings 6 30

getDbConn :: IO (H.Pool HP.Postgres)
getDbConn = do
    isEmpty <- isEmptyMVar poolMVar
    if isEmpty
        then do pool <- H.acquirePool postgresSettings poolSettings
                putMVar poolMVar pool
                return pool
        else readMVar poolMVar

toReason :: (Int,T.Text) -> Reason
toReason (rid,rtxt) = Reason rid rtxt

toTrip :: (Int,Int) -> Trip
toTrip (tid,rid) = Trip tid rid

toSource :: (Int,T.Text)
toSource (sid,stxt) = Source sid stxt

toPerson :: (Int,T.Text,Maybe T.Text,Maybe T.Text,Int)
toPerson (pid,na,ni,us,src) = Person pid na ni us src

toEntry :: (Int,Int,Day,Day,T.Text,Maybe T.Text)
toEntry (pid,tid,st,en,entry,img) = Entry pid tid st en entry img
