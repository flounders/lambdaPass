module Text.LambdaPass.Argument.Parser where

import Text.LambdaPass.Types
import Text.LambdaPass.Config 
     
import Control.Monad (join)
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

data Options = Options Filename Fingerprint KeyLocation Command

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

parseFile :: Maybe Filename -> Parser Filename
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
parseUsername = strOption $ short 'u' <> long "username" <> metavar "USERNAME"

parseLocation :: Parser Location
parseLocation = strOption $ short 'l' <> long "location" <> metavar "LOCATION"

parseNotes :: Parser Notes
parseNotes = strOption $ short 'n' <> long "notes" <> metavar "NOTES"

-- Commands

parseCommand :: Parser Command
parseCommand = subparser $
               command "add" (parseAdd `withInfo` "Add a username, location and password to your datastore.") <>
               command "view" (parseView `withInfo` "View account info.") <>
               command "update" (parseUpdate `withInfo` "Update account info.") <>
               command "remove" (parseRemove `withInfo` "Remove a username and password from the datastor.")

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
  <*> optional (strOption $ long "uU" <> long "updateUser" <> metavar "NEWUSER" 
                <> help "Update the username with the new username as the argument.")
  <*> optional ((\_ -> passPrompt) <$> switch (long "uP" <> long "updatePass"
                <> help "Update the password with a new password that will be prompted for."))
  <*> optional (strOption $ long "uL" <> long "updateLoc" <> metavar "NEWLOC"
                <> help "Update the location with the new location as the argument.")
  <*> optional (strOption $ long "uN" <> long "updateNotes" <> metavar "NEWNOTES"
                <> help "Update notes with new notes as the argument.")

passPrompt :: IO Password
passPrompt = do
  putStr "What password do you wish to store? "
  hFlush stdout
  hSetEcho stdin False
  p <- getLine
  hSetEcho stdin True
  return p

parseRemove :: Parser Command
parseRemove = Remove
              <$> optional parseUsername
              <*> optional parseLocation
              <*> optional parseNotes

-- Helper Functions
   
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
