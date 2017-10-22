module Main where

-- import Crypto1
-- import Crypto2
import Crypto3
import qualified Data.Vector as V

main :: IO ()


main = do
    print ( speckHexaPrueba ("6574","694c") ["1918","1110","0908","0100"] 20)
    print (mapTuple numericToHex (speckEncript (stringHexToNumeric "6574",stringHexToNumeric "694c") (V.fromList [stringHexToNumeric "1918",stringHexToNumeric "1110",stringHexToNumeric "0908",stringHexToNumeric "0100"]) 20 ))
    
