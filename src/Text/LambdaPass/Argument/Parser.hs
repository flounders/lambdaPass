{-# LANGUAGE OverloadedStrings #-}
module Text.LambdaPass.Argument.Parser where

import Text.LambdaPass.Types
import Text.LambdaPass.Config

import Control.Monad (join)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO

data AccountFields = UserField
  | PassField
  | LocField
  | NotesField
  deriving (Show, Eq, Ord)

data Command
  = Add Username (Maybe Location) (Maybe Notes)
  | View (Maybe Username) (Maybe Location) (Maybe Notes) [AccountFields]
  | ViewAll [AccountFields]
  | Update { selUser  :: Maybe Username
           , selLoc   :: Maybe Location
           , selNotes :: Maybe Notes
           , upUser   :: Maybe Username
           , upPass   :: Maybe (IO Password)
           , upLoc    :: Maybe Location
           , upNotes  :: Maybe Notes
           }
  | Remove (Maybe Username) (Maybe Location) (Maybe Notes)
  | Migrate

data Options = Options FilePath Fingerprint KeyLocation Command

-- General Options

parseOptions :: IO (Parser Options)
parseOptions = do
  baseConfPath <- getXdgDirectory XdgConfig "lambdaPass"
  createDirectoryIfMissing True baseConfPath
  let configPath = baseConfPath </> "lambdaPass.yml"
  configExists <- doesFileExist configPath
  if configExists then return () else do
    newFile <- openFile configPath AppendMode
    hPutStrLn newFile "base:"
    hClose newFile
  config <- loadConfig configPath
  return $ Options <$> parseFile (join . fmap (passFile . base) $ config)
                   <*> parseFingerprint (join . fmap (gpgKey . base) $ config)
                   <*> parseKey (join . fmap (gpgDir . base) $ config)
                   <*> parseCommand

parseFile :: Maybe FilePath -> Parser FilePath
parseFile fn =
  strOption $ short 'f' <> long "file" <> metavar "FILE" <> case fn of
                                                              Just x -> value x <> h
                                                              _      -> h
  where h = help "Encrypted file that contains passwords."

parseFingerprint :: Maybe Fingerprint -> Parser Fingerprint
parseFingerprint fpr =
  strOption $ short 'p' <> long "fpr" <> metavar "FINGERPRINT" <> case fpr of
                                                                    Just x -> value x <> h
                                                                    _      -> h
  where h = help "The fingerprint for the key you will use to encrypt the passwords file."

parseKey :: Maybe KeyLocation -> Parser KeyLocation
parseKey keyDir =
  strOption $ short 'k' <> long "key" <> metavar "GPGKEY" <> case keyDir of
                                                               Just x -> value x <> h
                                                               _      -> h
  where h = help "This is the path to where your GPG key is."

-- Option primitives

parseUsername :: Parser Username
parseUsername = fieldWrapperParser (Username) $ short 'u' <> long "username" <> metavar "USERNAME"

parseLocation :: Parser Location
parseLocation = fieldWrapperParser (Location) $ short 'l' <> long "location" <> metavar "LOCATION"

parseNotes :: Parser Notes
parseNotes = fieldWrapperParser (Notes) $ short 'n' <> long "notes" <> metavar "NOTES"

parseUpdateUsername :: Parser Username
parseUpdateUsername = fieldWrapperParser (Username) $ long "uU" <> long "updateUser" <> metavar "NEWUSER" <> help "Update the username with the new username as the argument."

parseUpdateLocation :: Parser Location
parseUpdateLocation = fieldWrapperParser (Location) $ long "uL" <> long "updateLoc" <> metavar "NEWLOC"
                <> help "Update the location with the new location as the argument."

parseUpdateNotes :: Parser Notes
parseUpdateNotes = fieldWrapperParser (Notes) $ long "uN" <> long "updateNotes" <> metavar "NEWNOTES" <> help "Update notes with new notes as the argument."

fieldWrapperParser :: (T.Text -> a) -> Mod OptionFields String -> Parser a
fieldWrapperParser c ms = c . T.pack <$> (strOption ms)

-- Commands

parseCommand :: Parser Command
parseCommand = subparser $
               command "add" (parseAdd `withInfo` "Add a username, location and password to your datastore.") <>
               command "view" (parseView `withInfo` "View account info.") <>
               command "update" (parseUpdate `withInfo` "Update account info.") <>
                 command "remove" (parseRemove `withInfo` "Remove a username and password from the datastor.") <>
               command "migrate" (parseMigrate `withInfo` "Migrate old account data from initial release to new data structure. Only use this for data created before 2016-10-18.")

parseAdd :: Parser Command
parseAdd = Add
           <$> parseUsername
           <*> optional parseLocation
           <*> optional parseNotes

parseView :: Parser Command
parseView =
  (View
  <$> optional parseUsername
  <*> optional parseLocation
  <*> optional parseNotes
  <*> parseFields "p")
  <|>
  (subparser $ command "all" (parserViewAll `withInfo` "View all accounts."))

parserViewAll :: Parser Command
parserViewAll =
  ViewAll
  <$> parseFields "ul"

-- parseFields takes a string that contains the default fields you wish to display.
-- It then returns the fields the user requested or that you set as default.
parseFields :: String -> Parser [AccountFields]
parseFields def =
  foldr f [] <$> strOption (short 'f' <> long "fields" <> value def <> metavar "FIELDS"
  <> help ("These are the account fields you wish displayed. Use 'u' for user, 'l' for location "
  ++ "'n' for notes and 'p' for password. Default behavior will be to just show password."))
  where f x acc = case x of
                    'u' -> UserField  : acc
                    'p' -> PassField  : acc
                    'l' -> LocField   : acc
                    'n' -> NotesField : acc
                    _ -> acc

parseUpdate :: Parser Command
parseUpdate =
  Update
  <$> optional parseUsername
  <*> optional parseLocation
  <*> optional parseNotes
  <*> optional parseUpdateUsername
  <*> optional ((\x -> case x of
                         True -> passPrompt
                         False -> return $ Password T.empty) <$> switch (long "uP" <> long "updatePass"
                <> help "Update the password with a new password that will be prompted for."))
  <*> optional parseUpdateLocation
  <*> optional parseUpdateNotes

parseMigrate :: Parser Command
parseMigrate = pure Migrate

passPrompt :: IO Password
passPrompt = do
  TIO.putStr "What password do you wish to store? "
  hFlush stdout
  hSetEcho stdin False
  p <- TIO.getLine
  hSetEcho stdin True
  return . Password $ p

parseRemove :: Parser Command
parseRemove = Remove
              <$> optional parseUsername
              <*> optional parseLocation
              <*> optional parseNotes

-- Helper Functions

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
