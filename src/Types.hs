{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Monad (mzero)
import Data.Aeson

type Filename    = String
type Fingerprint = String
type KeyLocation = String
type Username    = String
type Password    = String
type Location    = String
type Notes       = String

data Account = Account { username :: Username
                       , password :: Password
                       , location :: Location
                       , notes    :: Notes
                       } deriving (Eq, Show)

type Accounts = [Account]

instance FromJSON Account where
    parseJSON (Object v) = Account <$>
                           v .: "username" <*>
                           v .: "password" <*>
                           v .: "location" <*>
                           v .: "notes"
    parseJSON _          = mzero

instance ToJSON Account where
    toJSON (Account un pw l n) = object $
                                 [ "username" .= un
                                 , "password" .= pw
                                 , "location" .= l
                                 , "notes"    .= n
                                 ]
