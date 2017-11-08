module Crypto3 where

import Data.Either
import System.IO
import Codec.Picture
import Data.Bits
import Numeric
import qualified Data.Vector as V
import Data.Word
import Data.Maybe (listToMaybe,fromJust)
import Data.Char
import Test.QuickCheck
import Control.Monad

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

numericToHex aInt= showIntAtBase 16 intToDigit aInt ""

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

----------------------------------------------------------------
additionSpec :: Word16 -> Word16 -> Word16
additionSpec aWord anotherWord = aWord + anotherWord
substraccionSpeck :: Word16 -> Word16 -> Word16
substraccionSpeck aWord anotherWord = aWord - anotherWord

rotate16 :: Int -> Word16 -> Word16
rotate16 anAmount aWord = rotateR aWord anAmount

l :: V.Vector Word16 -> Word16 -> Word16
l keys t
  |t > (m - 2) = additionSpec (k keys i) (rotate16 8 (l keys i)) `xor` i
  |t <= (m - 2) = keys `V.unsafeIndex` (fromIntegral . toInteger) t
  where i = t - m + 1
        m = amountOfWordsForKey

k :: V.Vector Word16 -> Word16 -> Word16
k keys t
  |t == 0 = keys `V.unsafeIndex` (fromIntegral . toInteger) (m - 1)
  |otherwise = rotate16 (-3) (k keys i) `xor` l keys (i + m - 1)
  where i = t - 1
        m = amountOfWordsForKey

amountOfWordsForKey :: Word16
amountOfWordsForKey = 4

speckEncriptingRound (lBlock,rBlock) key = ((xor (additionSpec  (rotate16 8 lBlock) rBlock) key) ,xor (xor (additionSpec  (rotate16 8 lBlock) rBlock) key)  (rotate16 (-3) rBlock ))

speckEncript (lBlock,rBlock) keys rounds = foldl (\(lBlock,rBlock) round -> speckEncriptingRound (lBlock,rBlock) (k keys round)) (lBlock,rBlock)  [0..rounds]

speckDecriptRound (lBlock, rBlock) key = (rotate16 (-8) (substraccionSpeck (xor lBlock key) (rotate16 3 (xor lBlock rBlock))),rotate16 3 (xor lBlock rBlock))

speckDecript (lBlock,rBlock) keys rounds = foldl (\(lBlock,rBlock) round -> speckDecriptRound (lBlock,rBlock) (k keys round)) (lBlock,rBlock)  (reverse [0..rounds])


---------------------------------------------------------------
data Palabra16 = Palabra16 Word16 deriving Show
value (Palabra16 value)= value

instance Arbitrary Palabra16  where
  arbitrary  = Palabra16 `liftM` choose (40000, 65535)

keyVector=(V.fromList [stringHexToNumeric "1918",stringHexToNumeric "1110",stringHexToNumeric "0908", stringHexToNumeric "0100"])

speckPrueba:: (Palabra16,Palabra16)->Bool
speckPrueba (l,b) = (l1,b1)==(speckDecript (speckEncript (l1,b1) keyVector 5) keyVector 5)  
              where l1=rotate16 0 (value l)
                    b1=rotate16 0 (value b)



speckHexaPrueba (hex1,hex2) keys rounds = mapTuple numericToHex (speckDecript (speckEncript (stringHexToNumeric hex1, stringHexToNumeric hex2) (V.fromList (map stringHexToNumeric keys)) rounds) (V.fromList (map stringHexToNumeric keys)) rounds) 

--------------------------------------------------------------------------------------------------------------------------------------------------------------

word8To16:: Word8 ->Word16
word8To16  word1 = fromInteger (toInteger word1)

word8PairTo16Word word1 word2= (rotate16 (-8) (word8To16 word1)) + (word8To16 word2)

word16To8:: Word16 ->Word8
word16To8  word1 = fromInteger (mod (toInteger word1) 256)

word16ToPair8 :: Word16-> (Word8,Word8)
word16ToPair8 word1= (word16To8 (rotateR (word1 - (mod word1 256)) 8),word16To8 word1)

arrayOfWord8ToArrayOfWord16 :: [Word8]-> [Word16]
arrayOfWord8ToArrayOfWord16 (x1 : xs)
  |(length (x1  : xs)) >=2 = (word8PairTo16Word x1 (head xs)) : (arrayOfWord8ToArrayOfWord16 (tail xs))
  | otherwise =  (word8PairTo16Word x1 0) : (arrayOfWord8ToArrayOfWord16 (tail xs))

arrayOfWord8ToArrayOfWord16 []=[]

arrayOfWordToArrayOfTupla (x1:x2)
   | length (x1:x2)>=2 = (x1,(head x2)) : arrayOfWordToArrayOfTupla (tail x2)
   | otherwise =[(x1,0)]
arrayOfWordToArrayOfTupla [] = []

encryptImg arrayOfWord8 keys rounds = map (\tupla -> speckEncript tupla (V.fromList keys) rounds) ((arrayOfWordToArrayOfTupla.arrayOfWord8ToArrayOfWord16) arrayOfWord8)


--main =  quickCheck speckPrueba
