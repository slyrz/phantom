{-# LANGUAGE ForeignFunctionInterface #-}
module Phantom.Util
  (
    getPass
  ) where

import Foreign.C

foreign import ccall safe getpass :: CString -> IO CString

ansiEscLineUp  = "\x1b[F"
ansiEscLineClr = "\x1b[K"

clearLastLine :: IO()
clearLastLine =
  putStrLn $ ansiEscLineUp ++ ansiEscLineClr ++ ansiEscLineUp

getPass :: String -> IO String
getPass prompt = do
  result <- peekCString =<< getpass =<< newCString prompt
  clearLastLine
  return result
