module Crypto3 where

import Data.Bits
import Numeric
import qualified Data.Vector as V
import Data.Word
import Data.Maybe (listToMaybe,fromJust)
import Data.Char


intToBit aInt = showIntAtBase 2 intToDigit aInt ""

stringToArrayOfInt aStringBit = map digitToInt aStringBit

bitToInt :: Integral a => String ->  a
bitToInt = fromJust.fmap fst.listToMaybe.readInt 2 (`elem` "01") digitToInt

completeBits cant stringBits
  |length stringBits < cant = (head (reverse (take (cant+1-(length stringBits)) (iterate (++"0") "")))) ++ stringBits
  |otherwise = stringBits

completeByte stringAscii = completeBits  8 stringAscii

stringToAsciiChain aString = foldl1 (++) (map (completeByte.intToBit.ord) aString)
stringToNumber :: (Integral a) => String -> a
stringToNumber aString = bitToInt (stringToAsciiChain aString)

arrayRotate :: Int -> [a] -> [a]
arrayRotate _ [] = []
arrayRotate n xs
  |n > 0 = ( take (length xs). (reverse (take n (reverse xs)) ++)) xs
  |n < 0 = ( take (length xs). ((drop (n * (-1)) xs) ++))  xs

stringHexToBit string = foldl (++) "" (map (completeBits 4.intToBit) (map digitToInt string))

stringHexToNumeric string = bitToInt (stringHexToBit string)


----------------------------------------------------------------
additionSpec :: Word16 -> Word16 -> Word16
additionSpec aWord anotherWord = aWord + anotherWord
substraccionSpeck :: Word16 -> Word16 -> Word16
substraccionSpeck aWord anotherWord = aWord - anotherWord

rotate16 :: Int -> Word16 -> Word16
-- rotate16 anAmount aWord = (rotate aWord . negate) anAmount
rotate16 anAmount aWord = rotateR aWord anAmount

rotate8ToRight :: Word16 -> Word16
rotate8ToRight w = rotateR w 8

rotate3ToLeft :: Word16 -> Word16
rotate3ToLeft w = rotateL w 3

l :: V.Vector Word16 -> Word16 -> Word16
l keys t
  |t > (m - 2) = additionSpec (k keys i) (rotate16 8 (l keys i)) `xor` i
  |t <= (m - 2) = keys `V.unsafeIndex` (fromIntegral . toInteger) t
  where i = t - m + 1
        m = amountOfWordsForKey

k :: V.Vector Word16 -> Word16 -> Word16
k keys t
  |t == 0 = keys `V.unsafeIndex` (fromIntegral . toInteger) (m - 1)
  |otherwise = rotate3ToLeft (k keys i) `xor` l keys (i + m - 1)
  where i = t - 1
        m = amountOfWordsForKey

amountOfWordsForKey :: Word16
amountOfWordsForKey = 4

speckEncriptingRound (lBlock,rBlock) key = ((xor (additionSpec  (rotate16 8 lBlock) rBlock) key) ,xor (xor (additionSpec  (rotate16 8 lBlock) rBlock) key)  (rotate16 (-3) rBlock ))

speckEncript (lBlock,rBlock) keys rounds = foldl (\(lBlock,rBlock) round -> speckEncriptingRound (lBlock,rBlock) (k keys round)) (lBlock,rBlock)  [0..rounds]

speckDecriptRound (lBlock, rBlock) key = (rotate16 (-8) (substraccionSpeck (xor lBlock key) (rotate16 3 (xor lBlock rBlock))),rotate16 3 (xor lBlock rBlock))

speckDecript (lBlock,rBlock) keys rounds = foldl (\(lBlock,rBlock) round -> speckDecriptRound (lBlock,rBlock) (k keys round)) (lBlock,rBlock)  (reverse [0..rounds])
