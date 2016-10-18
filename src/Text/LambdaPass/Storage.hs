{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Text.LambdaPass.Storage where

import Text.LambdaPass.Types

import Crypto.Gpgme
import Control.Monad (mzero) -- Remove at 1.0.
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.String (fromString)

data Account = Account { accUsername :: Username
                       , accPassword :: Password
                       , accLocation :: Location
                       , accNotes    :: Notes
                       } deriving (Eq, Ord, Show)

type Accounts = [Account]

-- Remove at 1.0.
data OldAccount = OldAccount { oldAccUsername :: String
                             , oldAccPassword :: String
                             , oldAccLocation :: String
                             , oldAccNotes    :: String
                             } deriving (Eq, Ord, Show)

type OldAccounts = [OldAccount]

instance FromJSON OldAccount where
    parseJSON (Object v) = OldAccount <$>
      v .: "username" <*>
      v .: "password" <*>
      v .: "location" <*>
      v .: "notes"
    parseJSON _          = mzero

instance ToJSON OldAccount where
    toJSON (OldAccount un pw l n) = object $
      [ "username" .= un
      , "password" .= pw
      , "location" .= l
      , "notes"    .= n
      ]

deriveJSON defaultOptions ''Account

readStorageData' :: FromJSON a => FilePath
                 -> KeyLocation
                 -> IO (Either DecryptError [a])
readStorageData' fn key = do
  fContents <- B.readFile fn
  fileData <- decrypt' key fContents
  case fileData of
    Left x -> return $ Left x
    Right x -> return . Right $ f x
  where f x = case decode $ BL.fromStrict x of
                Just y -> y
                Nothing -> []

readStorageData :: FilePath
                -> KeyLocation
                -> IO (Either DecryptError Accounts)
readStorageData = readStorageData'

readOldStorageData :: FilePath
                   -> KeyLocation
                   -> IO (Either DecryptError OldAccounts)
readOldStorageData = readStorageData'

writeStorageData :: FilePath
                 -> KeyLocation
                 -> Fingerprint
                 -> Accounts
                 -> IO ()
writeStorageData fn key fpr accs = do
  results <- encrypt' key (fromString fpr) (BL.toStrict $ encode accs)
  case results of
    (Left e) -> putStrLn e
    (Right contents) -> B.writeFile fn contents
