module Phantom.Main
  (
      defaultMain
  )
where
import Control.Applicative ((<$>))
import Data.List (isPrefixOf)
import Phantom.Config
import Phantom.Random
import Phantom.Util

-- Convert an integer to a character from the password alphabet.
toPasswordChar :: Int -> Char
toPasswordChar =
  let
    chars = alphabet defaultConfig
  in
    (chars !!) . (`mod` length chars)

-- Generate the passwords for each entry.
generatePasswords :: State -> Int -> [String] -> [(String, String)]
generatePasswords _ _ [] = []
generatePasswords prng len (entry:entries) =
  let
    (prng', bytes) = takeRandom prng len
  in
    (entry, map (toPasswordChar . fromEnum) bytes) : generatePasswords prng' len entries

-- Pretty print the (name, password) pairs.
showPassword :: (String, String) -> IO()
showPassword (name, password) =
  let
    len = length name
    sep = replicate (max 1 (30 - len)) ' '
  in
    putStrLn $ name ++ sep ++ password

-- Ignore names starting with an underscore.
ignoreEntry :: String -> Bool
ignoreEntry name =
  isPrefixOf "_" name || null name

defaultMain = do
  -- Read password entries from file.
  entries <- lines <$> (readFile =<< path defaultConfig)
  -- Seed PRNG with master password.
  prng <- initRandom <$> getPass "Master-Password: "
  -- Generate passwords and filter blacked out entries. The order is crucial.
  -- Filtering before generation would affect existing passwords.
  let
    passwords = filter (not . ignoreEntry . fst) $ generatePasswords prng (size defaultConfig) entries
  mapM_ showPassword passwords
