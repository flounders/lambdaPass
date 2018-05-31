{-# LANGUAGE TemplateHaskell #-}
module Text.LambdaPass.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)

type Fingerprint = String
type KeyLocation = String
newtype Username = Username { username :: Text } deriving (Eq, Ord, Show)
newtype Password = Password { password :: Text } deriving (Eq, Ord, Show)
newtype Location = Location { location :: Text } deriving (Eq, Ord, Show)
newtype Notes = Notes { notes :: Text } deriving (Eq, Ord, Show)

data AccountSelector = AccountSelector { selectUsername :: Maybe Username
                                       , selectLocation :: Maybe Location
                                       , selectNotes :: Maybe Notes
                                       } deriving Show

concat <$> mapM (deriveJSON defaultOptions) [''Username, ''Password, ''Location, ''Notes] 
