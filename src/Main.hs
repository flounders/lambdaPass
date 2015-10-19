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
import Run.Control.Parser (parseRunControl, fileParse)

import Crypto.Gpgme
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.String
import Options.Applicative
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
             accounts <- readData file key
             newAccounts <- addAccount accounts u p l n
             case newAccounts of
               [] -> return ()
               _ -> writeData file key fpr newAccounts
    ViewAccount u l -> do
      accounts <- readData file key
      viewAccount accounts u l
    ViewPassword u l -> do
      accounts <- readData file key
      viewPassword accounts u l
    Remove u l -> do
      accounts <- readData file key
      let newAccounts = removeAccount accounts u l
      case newAccounts of
        Just xs -> writeData file key fpr xs
        Nothing -> putStrLn "Unable to delete account(s)."


-- Functions for operating on the password file

readData :: Filename
            -> KeyLocation
            -> IO (Either DecryptError Accounts)
readData fn key = do
  fileData <- readContents fn key
  case fileData of
    Left x -> return $ Left x
    Right x -> return . Right $ f x
  where f x = case decode $ BL.fromStrict x of
                Just y -> y
                Nothing -> []


writeData :: Filename
             -> KeyLocation
             -> Fingerprint
             -> Accounts
             -> IO ()
writeData fn key fpr accs = writeNewContents fn key fpr $ encode accs


viewPassword :: Either DecryptError Accounts
                -> Maybe Username
                -> Maybe Location
                -> IO ()
viewPassword (Left (NoData)) _  _  = return ()
viewPassword (Right xs) un loc = do
  let filteredAccounts = accountFiltering un loc (Just xs) (Just xs)
  errorCheck filteredAccounts
  where errorCheck (Nothing) = return () 
        errorCheck (Just ys) = putStrLn . (intercalate "\n\n") $ map password ys
viewPassword (Left x) _ _ = decryptErrorHandler x


viewAccount :: Either DecryptError Accounts
               -> Maybe Username
               -> Maybe Location
               -> IO ()
viewAccount (Left (NoData)) _ _ = return ()
viewAccount (Right xs) un loc = do
  let filteredAccounts = accountFiltering un loc (Just xs) (Just xs)
  errorCheck filteredAccounts 
  where errorCheck (Nothing) = return ()
        errorCheck (Just ys) = putStrLn . (intercalate "\n\n") $ map f ys
        f x = "Username: " ++ username x ++
              "\nPassword: " ++ password x ++
              "\nLocation: " ++ location x ++
              "\nNotes: " ++ notes x
viewAccount (Left x) _ _ = decryptErrorHandler x


addAccount :: Either DecryptError Accounts
              -> Username
              -> Password
              -> Location
              -> Notes
              -> IO Accounts
addAccount (Left (NoData)) un pass loc note = return $ appendPassword un pass loc note Nothing 
addAccount (Left x) _ _ _ _ = decryptErrorHandler x >> return []
addAccount (Right accs) un pass loc note = return $ appendPassword un pass loc note (Just accs)


appendPassword :: Username
               -> Password
               -> Location
               -> Notes
               -> Maybe Accounts
               -> Accounts
appendPassword un pass loc note (Nothing) = [(Account un pass loc note)]
appendPassword un pass loc note (Just xs) = xs ++ [(Account un pass loc note)]


removeAccount :: Either DecryptError Accounts
                 -> Maybe Username
                 -> Maybe Location
                 -> Maybe Accounts
removeAccount (Left _) _ _ = Nothing
removeAccount (Right xs) un loc = do
  filteredAccounts <- accountFiltering un loc (Just xs) (Just [])
  return $ filter (\x -> not (x `elem` filteredAccounts)) xs


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
