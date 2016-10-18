module Text.LambdaPass.Logic where

import Text.LambdaPass.Argument.Parser
import Text.LambdaPass.Storage
import Text.LambdaPass.Types

import Control.Monad (join)
import Crypto.Gpgme
import Data.List (sort)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Command router
run :: Options -> IO ()
run (Options file fpr key cmd) = do
  case cmd of
    Add u l n -> do
      accs <- readStorageData file key
      p <- passPrompt
      newAccs <- addAccount accs u p (maybe (Location T.empty) (id) l) (maybe (Notes T.empty) (id) n)
      writeStorageData file key fpr newAccs
    View u l n fields -> do
      accs <- readStorageData file key
      viewAccount accs u l n fields
    ViewAll fields -> do
      accs <- readStorageData file key
      viewAll accs fields
    Update sU sL sN uU uP uL uN -> do
      accs <- readStorageData file key
      nP <- sequence uP
      updatedAccs <- updateAccount accs sU sL sN uU nP uL uN
      writeStorageData file key fpr updatedAccs
    Remove u l n -> do
      accs <- readStorageData file key
      remainingAccs <- removeAccount accs u l n
      writeStorageData file key fpr remainingAccs
    Migrate -> do -- remove this command before 1.0
      accs <- readOldStorageData file key
      newAccs <- migrate accs
      writeStorageData file key fpr newAccs

-- Command back ends
addAccount :: Either DecryptError Accounts
           -> Username
           -> Password
           -> Location
           -> Notes
           -> IO Accounts
addAccount (Left x) un pass loc note = case x of
                                         NoData -> return [Account un pass loc note]
                                         _      -> decryptErrorHandler x >> return []
addAccount (Right accs) un pass loc note = return $ accs ++ [Account un pass loc note]

viewAccount :: Either DecryptError Accounts
            -> Maybe Username
            -> Maybe Location
            -> Maybe Notes
            -> [AccountFields]
            -> IO ()
viewAccount (Left e) _ _ _ _ = decryptErrorHandler e
viewAccount (Right accs) u l n fields = do
  let sel = accountFiltering u l n accs
  _ <- sequence . join . map (\x -> map (flip ($) x) (fieldDisplay fields)) $ sel
  return ()

fieldDisplay :: [AccountFields] -> [Account -> IO ()]
fieldDisplay fields = map f $ sort fields
  where f x = TIO.putStrLn . (case x of
                                UserField  -> username . accUsername
                                PassField  -> password . accPassword
                                LocField   -> location . accLocation
                                NotesField -> notes . accNotes)

viewAll :: Either DecryptError Accounts
        -> [AccountFields]
        -> IO ()
viewAll (Left e) _  = decryptErrorHandler e
viewAll (Right accs) fields = do
  _ <- sequence . join . map (\x -> map (flip ($) x) (fieldDisplay fields)) $ accs
  return ()

updateAccount :: Either DecryptError Accounts
              -> Maybe Username
              -> Maybe Location
              -> Maybe Notes
              -> Maybe Username
              -> Maybe Password
              -> Maybe Location
              -> Maybe Notes
              -> IO Accounts
updateAccount (Left e) _ _ _ _ _ _ _ = decryptErrorHandler e >> return []
updateAccount (Right accs) sU sL sN uU uP uL uN = return . ((++) unSelAccs) . map f $ selAccs
  where f (Account u p l n) = Account (maybeId u uU) (maybeId p uP) (maybeId l uL) (maybeId n uN)
        maybeId b a = maybe b (id) a
        selAccs = accountFiltering sU sL sN accs
        unSelAccs = filter (not . flip elem selAccs) accs

removeAccount :: Either DecryptError Accounts
              -> Maybe Username
              -> Maybe Location
              -> Maybe Notes
              -> IO Accounts
removeAccount (Left e) _ _ _ = decryptErrorHandler e >> return []
removeAccount (Right accs) u l n = return $ filter (\x -> not $ elem x accsToRemove) accs
  where accsToRemove = accountFiltering u l n accs

migrate :: Either DecryptError OldAccounts
        -> IO Accounts
migrate (Left e) = decryptErrorHandler e >> return []
migrate (Right accs) = return $ map f accs
  where f (OldAccount u p l n) = Account (Username . T.pack $ u) (Password . T.pack $ p) (Location . T.pack $ l) (Notes . T.pack $ n)

-- Common helper functions
decryptErrorHandler :: DecryptError -> IO ()
decryptErrorHandler x =
  putStrLn $ case x of
               NoData  -> "No data in the passwords file."
               BadPass -> "Wrong password. Please enter again."
               _       -> "Encountered an unhandled error."

accountFiltering :: Maybe Username
                 -> Maybe Location
                 -> Maybe Notes
                 -> Accounts
                 -> Accounts
accountFiltering u l n accs = concat . map (S.toList) . foldr f [] . filter (not . S.null) $ map (S.fromList) [filterBy (accUsername) u, filterBy (accLocation) l, filterBy (accNotes) n]
  where filterBy g = maybe [] (\x -> filter ((==) x . g) accs)
        f x [] = [x]
        f x (acc : []) = [S.intersection x acc]
        f _ _ = []

