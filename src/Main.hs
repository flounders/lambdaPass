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
    | ViewAll
    | Remove Username Location

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

run :: Options -> IO ()
run (Options file fpr key cmd) = do
  case cmd of
    Add u l n  -> do
             putStrLn "Type in the password you want to add: "
             p <- getLine
             addNewPassword file fpr key u p l n
    ViewAll    -> readPassword file key Nothing Nothing
    Remove u l -> removePassword file fpr key (Just u) (Just l)

testData :: Accounts
testData = [ (Account "swilliams" "password" "speedy" "none")
           , (Account "colonelflounders" "testing" "gmail" "none")
           ]

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

parseAdd :: Parser Command
parseAdd = Add
           <$> argument str (metavar "USERNAME")
           <*> argument str (metavar "LOCATION")
           <*> argument str (metavar "NOTES")

parseRemove :: Parser Command
parseRemove = Remove
              <$> argument str (metavar "USERNAME")
              <*> argument str (metavar "LOCATION")

parseCommand :: Parser Command
parseCommand = subparser $
               command "add" (parseAdd `withInfo` "Add a username, location and password to your datastore.") <>
               command "view" ((pure ViewAll) `withInfo` "View all accounts.") <>
               command "remove" (parseRemove `withInfo` "Remove a username and password from the datastor.")

{- read password

   get password file name
   open password file
   decrypt password file
   parse password file
   retrieve relevant password(s)
   print password(s)
   close file
-}

readPassword :: Filename
             -> KeyLocation
             -> Maybe Username
             -> Maybe Location
             -> IO ()
readPassword fn key un loc = do
  fContents <- B.readFile fn
  jsonData <- decrypt' (fromString key) fContents
  case jsonData of
    Left (NoData) -> return ()
    Left (BadPass) -> putStrLn "Wrong password. Please enter again."
    Right plaintext -> do
                  let accounts = decode $ BL.fromStrict plaintext
                  let filteredLocations = case loc of
                                            Nothing -> accounts
                                            Just x -> filter (\y -> x == location y) <$> accounts
                  let filteredAccounts = case un of 
                                           Nothing -> filteredLocations
                                           Just x -> filter (\y -> x == username y) <$> filteredLocations
                  errorCheck filteredAccounts
      where f x = "Username: " ++ username x ++ "\nPassword: " ++ password x ++ "\nLocation: " ++ location x ++ "\nNotes: " ++ notes x
            errorCheck (Nothing) = return ()
            errorCheck (Just xs) = putStrLn . (intercalate "\n\n") $ map f xs

{- add password

   get password file name
   open password file
   decrypt password file
   parse password file
   append new entry to structure
   encrypt structure
   write to file
   close file
-}

addNewPassword :: Filename
               -> Fingerprint
               -> KeyLocation
               -> Username
               -> Password
               -> Location
               -> Notes
               -> IO ()
addNewPassword fn fpr key user pass loc note = do
  fContents <- B.readFile fn
  jsonData <- decrypt' (fromString key) fContents
  case jsonData of
    Left (NoData) -> do
                  let newJSONdata = encode $ appendPassword user pass loc note Nothing
                  results <- encrypt' key (fromString fpr) (BL.toStrict newJSONdata)
                  errorCheck results
    Left (BadPass) -> putStrLn "Wrong password. Please enter again."
    Left _ -> putStrLn "Ran into an unhandled error."
    Right plaintext -> do
                  let newJSONdata = encode $ appendPassword user pass loc note (decode $ BL.fromStrict plaintext)
                  results <- encrypt' key (fromString fpr) (BL.toStrict newJSONdata)
                  errorCheck results
    where errorCheck (Left e) = putStrLn e
          errorCheck (Right message) = B.writeFile fn message


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
  fContents <- B.readFile fn
  jsonData <- decrypt' (fromString key) fContents
  case jsonData of
    Left (NoData) -> putStrLn "No passwords in file."
    Left (BadPass) -> putStrLn "Wrong password. Please enter again."
    Left _ -> putStrLn "Ran into an unhandled error."
    Right plaintext -> do
                  let accounts = (decode $ BL.fromStrict plaintext)
                  let filteredLocations = case loc of
                                            Nothing -> accounts
                                            Just x -> filter (\y -> x == location y) <$> accounts
                  let filteredAccounts = case un of 
                                           Nothing -> filteredLocations
                                           Just x -> filter (\y -> x == username y) <$> filteredLocations
                  let newJSONdata = encode $ (\ufs fs -> filter (\x -> not (x `elem` fs)) ufs) <$> accounts <*> filteredAccounts
                  results <- encrypt' key (fromString fpr) (BL.toStrict newJSONdata)
                  errorCheck results
    where errorCheck (Left e) = putStrLn e
          errorCheck (Right message) = B.writeFile fn message

