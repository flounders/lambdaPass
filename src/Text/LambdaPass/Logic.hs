{-# LANGUAGE OverloadedStrings #-}
module Text.LambdaPass.Logic where

import Text.LambdaPass.Argument.Parser
import Text.LambdaPass.Storage
import Text.LambdaPass.Types

import Control.Monad (join)
import Crypto.Gpgme
import Data.Either (either)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Command router
run :: Options
    -> IO ()
run (Options file fpr key cmd) =
  case cmd of
    Add u l n -> do
      accs <- readStorageData file key
      p <- passPrompt
      let newAccs = addAccount
                      accs
                      (Account
                        u
                        p
                        (fromMaybe (Location T.empty) l)
                        (fromMaybe (Notes T.empty) n))
      either (TIO.putStrLn) (writeStorageData file key fpr) newAccs
    View u l n fields -> do
      accs <- readStorageData file key
      viewAccount accs (AccountSelector u l n) fields
    ViewAll fields -> do
      accs <- readStorageData file key
      viewAll accs fields
    Update sU sL sN uU uP uL uN -> do
      accs <- readStorageData file key
      nP <- sequence uP
      let updatedAccs = updateAccount accs (AccountSelector sU sL sN) uU nP uL uN
      either (TIO.putStrLn) (writeStorageData file key fpr) updatedAccs
    Remove u l n -> do
      accs <- readStorageData file key
      let remainingAccs = removeAccount accs (AccountSelector u l n)
      either (TIO.putStrLn) (writeStorageData file key fpr) remainingAccs
    Migrate -- remove this command before 1.0
     -> do
      accs <- readOldStorageData file key
      let newAccs = migrate accs
      either (TIO.putStrLn) (writeStorageData file key fpr) newAccs

-- Command back ends
addAccount
  :: Either DecryptError Accounts
  -> Account
  -> Either Text Accounts
addAccount (Left x) account =
  case x of
    NoData -> Right [account]
    _ -> Left $ decryptErrorHandler x
addAccount (Right accs) account =
  return $ accs ++ [account]

viewAccount
  :: Either DecryptError Accounts
  -> AccountSelector
  -> [AccountFields]
  -> IO ()
viewAccount (Left e) _ _ = TIO.putStrLn $ decryptErrorHandler e
viewAccount (Right accs) selector fields = do
  let sel = accountFiltering selector accs
  _ <- sequence . join . map (\x -> map ($ x) (fieldDisplay fields)) $ sel
  return ()

fieldDisplay
  :: [AccountFields]
  -> [Account -> IO ()]
fieldDisplay fields = map f $ sort fields
  where
    f x =
      TIO.putStrLn .
      (case x of
         UserField -> username . accUsername
         PassField -> password . accPassword
         LocField -> location . accLocation
         NotesField -> notes . accNotes)

viewAll :: Either DecryptError Accounts
        -> [AccountFields] -> IO ()
viewAll (Left e) _ = TIO.putStrLn $ decryptErrorHandler e
viewAll (Right accs) fields = do
  _ <- sequence . join . map (\x -> map ($ x) (fieldDisplay fields)) $ accs
  return ()

updateAccount
  :: Either DecryptError Accounts
  -> AccountSelector
  -> Maybe Username
  -> Maybe Password
  -> Maybe Location
  -> Maybe Notes
  -> Either Text Accounts
updateAccount (Left e) _ _ _ _ _ = Left $ decryptErrorHandler e
updateAccount (Right accs) selector uU uP uL uN =
  Right . (unSelAccs ++) . map f $ selAccs
  where
    f (Account u p l n) =
      Account
        (fromMaybe u uU)
        (fromMaybe p uP)
        (fromMaybe l uL)
        (fromMaybe n uN)
    selAccs = accountFiltering selector accs
    unSelAccs = filter (not . flip elem selAccs) accs

removeAccount
  :: Either DecryptError Accounts
  -> AccountSelector
  -> Either Text Accounts
removeAccount (Left e) _ = Left $ decryptErrorHandler e
removeAccount (Right accs) selector = Right $ filter (`notElem` accsToRemove) accs
  where
    accsToRemove = accountFiltering selector accs

migrate
  :: Either DecryptError OldAccounts
  -> Either Text Accounts
migrate (Left e) = Left $ decryptErrorHandler e
migrate (Right accs) = Right $ map f accs
  where
    f (OldAccount u p l n) =
      Account (g Username u) (g Password p) (g Location l) (g Notes n)
    g h = h . T.pack

-- Common helper functions
decryptErrorHandler
  :: DecryptError
  -> Text
decryptErrorHandler x =
  case x of
    NoData -> "No data in the passwords file."
    BadPass -> "Wrong password. Please enter again."
    _ -> "Encountered an unhandled error."

accountFiltering
  :: AccountSelector
  -> Accounts
  -> Accounts
accountFiltering (AccountSelector u l n) accs =
  concatMap S.toList . foldr f [] . filter (not . S.null) $ map S.fromList xs
  where
    filterBy g = maybe [] (\x -> filter ((==) x . g) accs)
    f x [] = [x]
    f x [acc] = [S.intersection x acc]
    f _ _ = []
    xs = [filterBy accUsername u, filterBy accLocation l, filterBy accNotes n]
