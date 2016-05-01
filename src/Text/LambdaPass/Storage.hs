 {-# LANGUAGE OverloadedStrings #-}
 module Text.LambdaPass.Storage where

import Text.LambdaPass.Types

import Crypto.Gpgme
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.String (fromString)

data Account = Account { username :: Username
                       , password :: Password
                       , location :: Location
                       , notes    :: Notes
                       } deriving (Eq, Ord, Show)

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

readStorageData :: Filename
                -> KeyLocation 
                -> IO (Either DecryptError Accounts)
readStorageData fn key = do
  fContents <- B.readFile fn
  fileData <- decrypt' (fromString key) fContents
  case fileData of
    Left x -> return $ Left x
    Right x -> return . Right $ f x
  where f x = case decode $ BL.fromStrict x of
                Just y -> y
                Nothing -> []

writeStorageData :: Filename
                 -> KeyLocation
                 -> Fingerprint
                 -> Accounts
                 -> IO ()
writeStorageData fn key fpr accs = do
  results <- encrypt' key (fromString fpr) (BL.toStrict $ encode accs)
  case results of
    (Left e) -> putStrLn e
    (Right contents) -> B.writeFile fn contents
