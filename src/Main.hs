{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

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

import Types
import Command.Parser

import Crypto.Gpgme
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.String
import Options.Applicative
import RCParser (parseRunControl, fileParse)
import System.Directory (doesFileExist)
import System.IO

main :: IO ()
main = do
  exists <- doesFileExist rcPath 
  case exists of
    True -> do
      withRC rcPath
    False -> do
      withoutRC
  where rcPath = "/home/swilliams/.lambdaPassrc"
        withRC path = do
          results <- fileParse parseRunControl path
          case results of
            Left _ -> putStrLn ("Encountered an error parsing " ++ path) >> withoutRC
            Right xs -> run =<< execParser
                        (parseOptionsFromRC (optionsFromRC xs) `withInfo` desc)
        withoutRC = run =<< execParser
                    (parseOptions `withInfo` desc)
        pairExtract x xs = snd . head $ filter (\y -> x == (fst y)) xs
        optionsFromRC xs = ( pairExtract "file" xs
                           , pairExtract "fingerprint" xs
                           , pairExtract "keydir" xs
                           )
        desc = "Password manager using GPG, JSON and most importantly Haskell."

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
    ViewAccount u l -> viewAccount file key u l
    ViewPassword u l -> viewPassword file key u l
    Remove u l -> removePassword file fpr key u l

-- Functions for operating on the password file

viewPassword :: Filename
                -> KeyLocation
                -> Maybe Username
                -> Maybe Location
                -> IO ()
viewPassword fn key un loc = do
             jsonData <- readContents fn key
             case jsonData of
               Left (NoData) -> return ()
               Left x -> decryptErrorHandler x
               Right plaintext -> do
                     let accounts = decode $ BL.fromStrict plaintext
                     let filteredAccounts = accountFiltering un loc accounts accounts
                     errorCheck filteredAccounts
                 where f x = password x
                       errorCheck (Nothing) = return ()
                       errorCheck (Just xs) = putStrLn . (intercalate "\n\n") $ map f xs

viewAccount :: Filename
               -> KeyLocation
               -> Maybe Username
               -> Maybe Location
               -> IO ()
viewAccount fn key un loc = do
  jsonData <- readContents fn key
  case jsonData of
    Left (NoData) -> return ()
    Left x -> decryptErrorHandler x
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
    Left x -> decryptErrorHandler x
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
    Left z -> decryptErrorHandler z
    Right plaintext -> do
                  let accounts = (decode $ BL.fromStrict plaintext)
                  let filteredAccounts = accountFiltering un loc accounts (Just [])
                  let newJSONdata = encode $ (\ufs fs -> filter (\x -> not (x `elem` fs)) ufs) <$> accounts <*> filteredAccounts
                  writeNewContents fn key fpr newJSONdata

decryptErrorHandler :: DecryptError -> IO ()
decryptErrorHandler (BadPass) = putStrLn "Wrong password. Please enter again."
decryptErrorHandler _         = putStrLn "Encountered an unhandled error."

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
