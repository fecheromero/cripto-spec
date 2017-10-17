
import Data.Bits
import Numeric
import Data.Maybe (listToMaybe,fromJust)
import Data.Char
import Data.Text.Encoding
import Data.Text(pack)




intToBit aInt = showIntAtBase 2 intToDigit aInt ""

stringToArrayOfInt aStringBit = map digitToInt aStringBit

bitToInt :: Integral a => String ->  a
bitToInt = fromJust.fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

completeBits cant stringBits 
	|length stringBits<cant= (head (reverse (take (cant+1-(length stringBits)) (iterate (++"0") "")))) ++ stringBits
	|otherwise=stringBits

completeByte stringAscii= completeBits  8 stringAscii

stringToAsciiChain aString=foldl1 (++) (map (completeByte.intToBit.ord) aString)
stringToNumber:: (Integral a)=> String->a
stringToNumber aString= bitToInt (stringToAsciiChain aString)

arrayRotate :: Int -> [a] -> [a]
arrayRotate _ [] = []
arrayRotate n xs 
			|n>0 = ( take (length xs). (reverse (take n (reverse xs)) ++)) xs
			|n<0 = ( take (length xs). ((drop (n * (-1)) xs) ++))  xs

bitRotate rotation number size=  bitToInt (arrayRotate rotation (completeBits  size (intToBit number)))



additionModule value1 value2 moduleValue= mod (value1 + value2) moduleValue


----------------------------------------------------------------

additionSpec value1 value2 = additionModule value1  value2  (2^16)

stringHexToBit string=foldl (++) "" (map (completeBits 4.intToBit) (map digitToInt string))

stringHexToNumeric string= bitToInt (stringHexToBit string)

s_a value=bitRotate (8) value 16
s_b value=bitRotate (-3) value 16

l:: (Num a, Show a,Integral a, Bits a)=> [a]->a->a
l keys t
	|t>(m-2) = xor (additionSpec (k keys i) (s_a (l keys i))) ( i)
	|t<=(m-2) = keys!!(fromIntegral t)
	where i = t-m+1
	      m = 4

k:: (Num a, Show a,Integral a, Bits a)=> [a]->a->a
k keys t
	|t==0 = keys!!(fromIntegral (m-1))
	|otherwise = xor (s_b (k keys i)) (l keys (i+m-1))
	where i = t-1
	      m = 4

speckEncriptingRound (lBlock,rBlock) key =((xor (additionSpec  (s_a lBlock) rBlock) key) ,xor (xor (additionSpec  (s_a lBlock) rBlock) key)  (s_b rBlock ))

speckEncript (lBlock,rBlock) keys rounds=foldl (\(lBlock,rBlock) round->speckEncriptingRound (lBlock,rBlock) (k keys round)) (lBlock,rBlock)  [0..rounds] 