module Autonix.Regex (match, makeRegex) where

import Data.ByteString (ByteString)
import Text.Regex.TDFA (Regex)
import qualified Text.Regex.TDFA as R

match :: Regex -> ByteString -> [[ByteString]]
match = R.match

makeRegex :: ByteString -> Regex
makeRegex = R.makeRegex
