module Utils.Days where

import Import

showDate :: Day -> String
showDate d = let (year, month, day) = toGregorian d
             in show month ++ "/" ++ show day ++ "/" ++ show year
