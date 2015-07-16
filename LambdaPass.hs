import Crypto.Gpgme

type Password = String
type Username = String
type Location = String

data Account = Account { location :: Location
                       , username :: Username
                       , password :: Password
                       } deriving (Show)

main = return ()
