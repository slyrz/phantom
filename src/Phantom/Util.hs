{-# LANGUAGE ForeignFunctionInterface #-}
module Phantom.Util
  (
      getPass
    , textBold
    , textColored
  ) where

import Foreign.C

foreign import ccall safe getpass :: CString -> IO CString

ansiEscLineClr   = "\x1b[K"
ansiEscLineUp    = "\x1b[F"
ansiEscTextBold  = "\x1b[1m"
ansiEscTextColor = "\x1b[33m"
ansiEscTextNorm  = "\x1b[0m"

clearLastLine :: IO()
clearLastLine =
  putStrLn $ ansiEscLineUp ++ ansiEscLineClr ++ ansiEscLineUp

getPass :: String -> IO String
getPass prompt = do
  result <- peekCString =<< getpass =<< newCString prompt
  clearLastLine
  return result

textBold :: String -> String
textBold text =
  ansiEscTextBold ++ text ++ ansiEscTextNorm

textColored :: String -> String
textColored text =
  ansiEscTextColor ++ text ++ ansiEscTextNorm
