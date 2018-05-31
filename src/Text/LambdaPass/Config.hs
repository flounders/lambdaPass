{-# LANGUAGE OverloadedStrings #-}

module Text.LambdaPass.Config where

import qualified Data.Aeson.Types as AT
import Data.Yaml
import Text.LambdaPass.Types

newtype Config = Config
  { base :: BaseConfig
  } deriving (Show)

data BaseConfig = BaseConfig
  { gpgDir :: Maybe FilePath
  , gpgKey :: Maybe Fingerprint
  , passFile :: Maybe FilePath
  } deriving (Show)

instance FromJSON Config where
  parseJSON (Object v) = Config <$> v .: "base"
  parseJSON invalid = AT.typeMismatch "Config" invalid

instance FromJSON BaseConfig where
  parseJSON (Object v) =
    BaseConfig <$> v .:? "gpgDir" <*> v .:? "gpgKey" <*> v .:? "passFile"
  parseJSON invalid = AT.typeMismatch "BaseConfig" invalid

loadConfig
  :: FilePath
  -> IO (Maybe Config)
loadConfig = decodeFile
