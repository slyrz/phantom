module Phantom.Config
  (
      Context
    , alphabet
    , size
    , path
    , defaultConfig
  )
where
import Control.Applicative ((<$>))
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

data Context = Context
  { alphabet :: !String     -- Password alphabet.
  , size     :: !Int        -- Password length.
  , path     :: IO FilePath -- Path to password entry file.
  }

defaultConfig :: Context
defaultConfig = Context
  { alphabet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  , size = 12
  , path = (</> ".phantoms") <$> getHomeDirectory
  }
