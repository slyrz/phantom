-- This module utilizes the RC4 stream cipher to generate random numbers.
module Phantom.Random
  (
    State
  , initRC4
  , takeRC4
  , initRandom
  , takeRandom
  , hash
  ) where

import Data.Array (Array, listArray, (!), (//))
import Data.Bits ((.&.))
import Data.Word (Word8)
import qualified Data.ByteString as BS (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as BC (pack)
import qualified Crypto.Hash.Whirlpool as CH (hash)
import Phantom.Config (repeats, defaultConfig)

data State = State
  { s ::  Array Int Word8
  , i :: !Int
  , j :: !Int
  } deriving (Show)

-- Creates an infinite list of hash values, where every item is is calculated
-- by applying the hash function on the previous item.
hash :: BS.ByteString -> [[Word8]]
hash val =
  map BS.unpack $ iterate CH.hash val

-- Swap array elements at index i and j.
swapIdx :: Array Int a -> Int -> Int -> Array Int a
swapIdx arr i j =
  arr // [ (i, arr ! j), (j, arr ! i) ]

takeIdx :: (Enum a) => Array Int a -> Int -> Int -> a
takeIdx arr i j =
  arr ! ((fromEnum (arr ! i) + fromEnum (arr ! j)) .&. 0xff)

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
initRC4' :: State -> [Word8] -> State
initRC4' state (k:xk) =
  if i state >= 0 && i state <= 255
    then
      let
        i' = (i state + 1)
        j' = (j state + fromEnum (s state ! i state) + fromEnum k) .&. 0xff
        s' = swapIdx (s state) (i state) j'
      in
        initRC4' state { s = s', i = i', j = j' } xk
    else
      state { i = 0, j = 0 }

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
takeRC4' :: State -> Int -> [Word8] -> (State, [Word8])
takeRC4' state n sample =
  if n > 0
    then
      let
        i' = (i state + 1) .&. 0xff
        j' = (j state + fromEnum (s state ! i')) .&. 0xff
        s' = swapIdx (s state) i' j'
        n' = n - 1
      in
        takeRC4' state {s = s', i = i', j = j'} n' (sample ++ [takeIdx s' i' j'])
    else
      (state, sample)

initRC4 :: [Word8] -> State
initRC4 key =
  initRC4' State {s = listArray (0, 255) [ 0..255 ], i = 0, j = 0} (concat . repeat $ key)

takeRC4 :: State -> Int -> (State, [Word8])
takeRC4 state n =
  takeRC4' state n []

initRandom :: String -> State
initRandom key =
  initRC4 . concat . drop (repeats defaultConfig) . hash $ BC.pack key

takeRandom :: State -> Int -> (State, [Word8])
takeRandom state n =
  let
    (state', sample) = takeRC4 state 128
  in
    (state', take n . (!! repeats defaultConfig) . hash $ BS.pack sample)
