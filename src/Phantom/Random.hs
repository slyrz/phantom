-- This module utilizes the RC4 stream cipher to generate random numbers.
module Phantom.Random
  (
    State
  , initRC4
  , takeRC4
  , initRandom
  , takeRandom
  ) where

import Data.Array (Array, listArray, (!), (//))
import Data.Word (Word8)
import qualified Data.ByteString as BS (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as BC (pack)
import qualified Crypto.Hash.Whirlpool as CH (hash)
import Phantom.Config (repeats, defaultConfig)

type State = Array Int Word8

-- Repeated hash of value.
hash :: BS.ByteString -> [[Word8]]
hash val =
  map BS.unpack $ val : iterate CH.hash val

-- Key-Scheduling Algorithm
--
--   for i from 0 to 255
--       S[i] := i
--   endfor
--   j := 0
--   for i from 0 to 255
--       j := (j + S[i] + key[i mod keylength]) mod 256
--       swap values of S[i] and S[j]
--   endfor
--
init' :: Int -> Int -> [Word8] -> State -> State
init' i j (k:xk) s =
  if i >= 0 && i <= 255
    then
      let
        i' = (i + 1)
        j' = (j + fromEnum (s!i) + fromEnum k) `mod` 256
      in
        init' i' j' xk (s // [ (i, s!j'), (j', s!i) ])
    else
      s

-- Pseudo-Random Generation Algorithm
--
--   i := 0
--   j := 0
--   while GeneratingOutput:
--       i := (i + 1) mod 256
--       j := (j + S[i]) mod 256
--       swap values of S[i] and S[j]
--       K := S[(S[i] + S[j]) mod 256]
--       output K
--   endwhile
--
take' :: Int -> Int -> Int -> [Word8] -> State -> (State, [Word8])
take' i j n k s =
  if n > 0
    then
      let
        i' = (i + 1) `mod` 256
        j' = (j + fromEnum (s!i')) `mod` 256
        s' = (s // [ (i', s!j'), (j', s!i') ])
        l' = (s'!((fromEnum (s'!i') + fromEnum (s'!j')) `mod` 256))
        n' = n - 1
      in
        take' i' j' n' (k ++ [l']) s'
    else
      (s, k)

initRC4 :: [Word8] -> Array Int Word8
initRC4 key =
  let
    s = listArray (0, 255) [ 0..255 ]
    k = concat . repeat $ key
  in
    init' 0 0 k s

takeRC4 :: State -> Int -> (State, [Word8])
takeRC4 state n =
  take' 0 0 n [] state

initRandom :: String -> Array Int Word8
initRandom key =
  initRC4 . (!! repeats defaultConfig) . hash $ BC.pack key

takeRandom :: State -> Int -> (State, [Word8])
takeRandom state n =
  let
    (state', randomWords) = takeRC4 state 128
  in
    (state', take n . (!! repeats defaultConfig) . hash $ BS.pack randomWords)
