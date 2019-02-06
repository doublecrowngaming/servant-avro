{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Servant.API.ContentTypes.Avro (Avro) where

import           Data.Avro                (FromAvro, Result (..), ToAvro,
                                           decode, encode)
import           Data.Proxy               (Proxy (..))
import           Data.Typeable            (Typeable)
import           Servant.API.ContentTypes

-- | The Avro encoding type for Servant
data Avro
  deriving Typeable

instance Accept Avro where
  -- The content type is given by https://avro.apache.org/docs/1.8.1/spec.html#HTTP+as+Transport
  contentType Proxy = "avro/binary"

instance ToAvro a => MimeRender Avro a where
  mimeRender Proxy = encode

instance FromAvro a => MimeUnrender Avro a where
  mimeUnrender Proxy encoded =
    case decode encoded of
      Error errstr -> Left errstr
      Success val  -> Right val
