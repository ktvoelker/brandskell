module Utils.Database where

import Import
import Data.Maybe

import qualified Hasql as H
import qualified Hasql.Postgres as HP

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
getDbConn = H.acquirePool postgresSettings poolSettings
