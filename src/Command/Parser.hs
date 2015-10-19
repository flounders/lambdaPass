module Command.Parser where

import Types
     
import Options.Applicative

data Command
    = Add Username Location Notes
    | ViewAccount (Maybe Username) (Maybe Location)
    | ViewPassword (Maybe Username) (Maybe Location)
    | Remove (Maybe Username) (Maybe Location)

data Options = Options Filename Fingerprint KeyLocation Command

-- General Options

parseOptions :: Parser Options
parseOptions = Options <$> parseFile <*> parseFingerprint <*> parseKey <*> parseCommand

parseOptionsFromRC :: (Filename, Fingerprint, KeyLocation) -> Parser Options
parseOptionsFromRC (fn, fpr, key) = (Options fn fpr key) <$> parseCommand

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


-- Commands

parseCommand :: Parser Command
parseCommand = subparser $
               command "add" (parseAdd `withInfo` "Add a username, location and password to your datastore.") <>
               command "view" (parseView `withInfo` "View account info.") <>
               command "remove" (parseRemove `withInfo` "Remove a username and password from the datastor.")

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
parseView = subparser $
  command "all" (parseViewAccount `withInfo` "View all account information. (e.g., username, password, email, notes)")
  <> command "pass" (parseViewPassword `withInfo` "View just the password.")

parseViewAccount :: Parser Command
parseViewAccount = ViewAccount
  <$> parseUsername
  <*> parseLocation

parseViewPassword :: Parser Command
parseViewPassword = ViewPassword
  <$> parseUsername
  <*> parseLocation

-- Helper Functions
   
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc
