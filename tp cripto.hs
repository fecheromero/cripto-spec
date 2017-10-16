{-# LANGUAGE FlexibleContexts #-}
import Data.Bits
import Numeric
import Data.Char
import Data.Text.Encoding
import Data.Text(pack)


(x!y)m|odd y=z q|y>0=q|0<1=1where z 0=0;z n=until(<m)(+(-m))$x+z(n-1);q=(z x!div y 2)m


intToBit aInt = showIntAtBase 2 intToDigit aInt ""

stringToArrayOfInt aStringBit = map digitToInt aStringBit

--bitToInt:: (Integral a) => String->a 
bitToInt aBit 
			| length aBit >1 = fromIntegral ((head $ stringToArrayOfInt aBit)  * 2^(length aBit-1) + bitToInt  (tail aBit))
			| otherwise = fromIntegral (head $ stringToArrayOfInt aBit )

completeBits stringBits cant
	|length stringBits<cant= (head (reverse (take (cant+1-(length stringBits)) (iterate (++"0") "")))) ++ stringBits
	|otherwise=stringBits

completeByte stringAscii= completeBits stringAscii 8

stringToAsciiChain aString=foldl1 (++) (map (completeByte.intToBit.ord) aString)
stringToNumber aString= bitToInt (stringToAsciiChain aString)

arrayRotate :: Int -> [a] -> [a]
arrayRotate _ [] = []
arrayRotate n xs 
			|n>0 = ( take (length xs). (reverse (take n (reverse xs)) ++)) xs
			|n<0 = ( take (length xs). ((drop (n * (-1)) xs) ++))  xs

bitRotate rotation number size=  bitToInt (arrayRotate rotation (completeBits (intToBit number) size))


additionModule value1 value2 moduleValue= mod (value1+value2) moduleValue --((value1+value2)!1) moduleValue

additionSpec value1 value2 =additionModule  value1  value2  (2^96)

roundSpecBlock (lBlock, rBlock) key =((xor (additionSpec (bitRotate 8 lBlock 96) rBlock) key) ,xor (xor (additionSpec (bitRotate 8 lBlock 96) rBlock) key) (bitRotate (-3) rBlock 96))

keyScheduleRound (lKey,rKey) numberOfRound
				|numberOfRound==0 =((xor (additionSpec  (bitRotate 8 lKey 96) rKey) numberOfRound),rKey)
				|otherwise =((xor (additionSpec  (bitRotate 8 lKey 96) rKey) numberOfRound),xor (xor (additionSpec  (bitRotate 8 lKey 96) rKey) numberOfRound) (bitRotate (-3) rKey 96))

keyRecursiveRound (lKey,rKey) numberOfRounds=foldl keyScheduleRound (lKey,rKey) [0..numberOfRounds]

roundSpec block (lKey,rKey) roundNumber =(roundSpecBlock block rKey,keyRecursiveRound (lKey,rKey) (roundNumber+1))


roundSpecRecursive block key numberOfRounds=foldl (\(block,key) round->roundSpec block key round) (block,key) [0..numberOfRounds]



