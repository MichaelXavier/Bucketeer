module Bucketeer.Testing.TestHelpers (toJSONText,
                                      decodeJSON) where

import Data.Aeson (json',
                   Result(..),
                   FromJSON,
                   fromJSON)
import Data.Aeson.Encode (fromValue)
import Data.Attoparsec.Lazy (eitherResult,
                             parse)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text(..))
import Data.Text.Lazy.Builder (toLazyText)
import Data.Aeson.Types (ToJSON,
                         toJSON)

toJSONText :: ToJSON a
              => a
              -> Text
toJSONText = toLazyText . fromValue . toJSON

decodeJSON :: FromJSON a
             => ByteString
             -> Either String a
decodeJSON str = fjson =<< parsed
  where parsed  = eitherResult . parse json' $ str
        fjson v = case fromJSON v of
                    Success x -> Right x
                    Error e   -> Left e
