-- This module utilizes the RC4 stream cipher to generate random numbers.
module Phantom.Random
  (
    State
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

-- Hash value n times.
hash :: BS.ByteString -> [Word8]
hash val =
  BS.unpack (iterate CH.hash val !! repeats defaultConfig)

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
        l' = (s!((fromEnum (s!i') + fromEnum (s!j')) `mod` 256))
        n' = n - 1
      in
        take' i' j' n' (k ++ [l']) (s // [ (i', s!j'), (j', s!i') ])
    else
      (s, hash $ BS.pack k)

initRandom :: String -> Array Int Word8
initRandom key =
  let
    i = 0
    j = 0
    s = listArray (0, 255) [ 0..255 ]
    k = concat . repeat . hash $ BC.pack key
  in
    init' i j k s

takeRandom :: State -> Int -> (State, [Word8])
takeRandom s n =
  let
    (s', r) = take' 0 0 256 [] s
  in
    (s', take n r)