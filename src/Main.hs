{-# LANGUAGE OverloadedStrings #-}

{- LambdaPass - A password manager using GPGME, JSON and Haskell
   Copyright (C) 2015 Steven Williams

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License
   as published by the Free Software Foundation; either version 2
   of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA. -}

module Main where

import Control.Monad -- for mzero
import Crypto.Gpgme
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.String
import Options.Applicative
import System.IO

type Filename    = String
type Fingerprint = String
type KeyLocation = String
type Username    = String
type Password    = String
type Location    = String
type Notes       = String

-- Option Parser datatypes
data Command
    = Add Username Location Notes
    | View (Maybe Username) (Maybe Location)
    | Remove (Maybe Username) (Maybe Location)

data Options = Options Filename Fingerprint KeyLocation Command

-- JSON file specific datatypes
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

main :: IO ()
main = run =<< execParser
       (parseOptions `withInfo` "Password manager using GPG, JSON and most importantly Haskell.")

-- Command line option parsing

run :: Options -> IO ()
run (Options file fpr key cmd) = do
  case cmd of
    Add u l n -> do
             putStrLn "Type in the password you want to add: "
             hFlush stdout
             hSetEcho stdin False
             p <- getLine
             hSetEcho stdin True
             addNewPassword file fpr key u p l n
    View u l -> readPassword file key u l
    Remove u l -> removePassword file fpr key u l

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseOptions :: Parser Options
parseOptions = Options <$> parseFile <*> parseFingerprint <*> parseKey <*> parseCommand

parseFile :: Parser Filename
parseFile = strOption $
            short 'f' <> long "file" <> metavar "FILE" <>
            help "Encrypted file that contains passwords."

parseFingerprint :: Parser Fingerprint
parseFingerprint = strOption $
                   short 'p' <> long "fpr" <> metavar "FINGERPRINT" <>
                   help "Fingerprint for the key you will use to encrypt the passwords file."

parseKey :: Parser KeyLocation
parseKey = strOption $
           short 'k' <> long "key" <> metavar "GPGKEY" <>
           help "This is the path to where your GPG key is."

parseUsername :: Parser (Maybe Username)
parseUsername = optional $ strOption $ short 'u' <> long "username" <> metavar "USERNAME"

parseLocation :: Parser (Maybe Location)
parseLocation = optional $ strOption $ short 'l' <> long "location" <> metavar "LOCATION"

parseAdd :: Parser Command
parseAdd = Add
           <$> argument str (metavar "USERNAME")
           <*> argument str (metavar "LOCATION")
           <*> argument str (metavar "NOTES")

parseRemove :: Parser Command
parseRemove = Remove
              <$> parseUsername
              <*> parseLocation

parseView :: Parser Command
parseView = View
            <$> parseUsername
            <*> parseLocation

parseCommand :: Parser Command
parseCommand = subparser $
               command "add" (parseAdd `withInfo` "Add a username, location and password to your datastore.") <>
               command "view" (parseView `withInfo` "View all accounts.") <>
               command "remove" (parseRemove `withInfo` "Remove a username and password from the datastor.")

-- Functions for operating on the password file

readPassword :: Filename
             -> KeyLocation
             -> Maybe Username
             -> Maybe Location
             -> IO ()
readPassword fn key un loc = do
  jsonData <- readContents fn key
  case jsonData of
    Left (NoData) -> return ()
    Left _ -> decryptErrorHandler jsonData
    Right plaintext -> do
                  let accounts = decode $ BL.fromStrict plaintext
                  let filteredAccounts = accountFiltering un loc accounts accounts
                  errorCheck filteredAccounts
      where f x = "Username: " ++ username x ++ "\nPassword: " ++ password x ++ "\nLocation: " ++ location x ++ "\nNotes: " ++ notes x
            errorCheck (Nothing) = return ()
            errorCheck (Just xs) = putStrLn . (intercalate "\n\n") $ map f xs

addNewPassword :: Filename
               -> Fingerprint
               -> KeyLocation
               -> Username
               -> Password
               -> Location
               -> Notes
               -> IO ()
addNewPassword fn fpr key user pass loc note = do
  jsonData <- readContents fn key
  case jsonData of
    Left (NoData) -> do
                  let newJSONdata = encode $ appendPassword user pass loc note Nothing
                  writeNewContents fn key fpr newJSONdata
    Left _ -> decryptErrorHandler jsonData
    Right plaintext -> do
                  let newJSONdata = encode $ appendPassword user pass loc note (decode $ BL.fromStrict plaintext)
                  writeNewContents fn key fpr newJSONdata


appendPassword :: Username
               -> Password
               -> Location
               -> Notes
               -> Maybe Accounts
               -> Accounts
appendPassword un pass loc note (Nothing) = [(Account un pass loc note)]
appendPassword un pass loc note (Just xs) = xs ++ [(Account un pass loc note)]


removePassword :: Filename
               -> Fingerprint
               -> KeyLocation
               -> Maybe Username
               -> Maybe Location
               -> IO ()
removePassword fn fpr key un loc = do
  jsonData <- readContents fn key
  case jsonData of
    Left (NoData) -> putStrLn "No passwords in file."
    Left _ -> decryptErrorHandler jsonData
    Right plaintext -> do
                  let accounts = (decode $ BL.fromStrict plaintext)
                  let filteredAccounts = accountFiltering un loc accounts (Just [])
                  let newJSONdata = encode $ (\ufs fs -> filter (\x -> not (x `elem` fs)) ufs) <$> accounts <*> filteredAccounts
                  writeNewContents fn key fpr newJSONdata

decryptErrorHandler :: Either DecryptError Plain -> IO ()
decryptErrorHandler (Left (BadPass)) = putStrLn "Wrong password. Please enter again."
decryptErrorHandler (Left _)         = putStrLn "Encountered an unhandled error."
decryptErrorHandler _                = putStrLn "Something is up with Haskell's pattern matching if you see this."

accountFiltering :: Maybe Username
                 -> Maybe Location 
                 -> Maybe Accounts
                 -> Maybe Accounts
                 -> Maybe Accounts
accountFiltering (Nothing) (Nothing) _ defaultValue = defaultValue
accountFiltering (Just u) (Nothing) accounts _ = filter (\x -> u == username x) <$> accounts
accountFiltering (Nothing) (Just l) accounts _ = filter (\x -> l == location x) <$> accounts
accountFiltering (Just u) (Just l) accounts _ = filter (\x -> u == username x && l == location x) <$> accounts

readContents :: Filename
             -> KeyLocation
             -> IO (Either DecryptError Plain)
readContents fn key = do
  fContents <- B.readFile fn
  decrypt' (fromString key) fContents

writeNewContents :: Filename
                 -> KeyLocation
                 -> Fingerprint
                 -> BL.ByteString
                 -> IO ()
writeNewContents fn key fpr contents = do
  results <- encrypt' key (fromString fpr) (BL.toStrict contents)
  errorCheck results
    where errorCheck (Left e) = putStrLn e
          errorCheck (Right message) = B.writeFile fn message
