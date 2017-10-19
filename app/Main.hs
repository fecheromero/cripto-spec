module Main where

-- import Crypto1
-- import Crypto2
import Crypto3
import qualified Data.Vector as V

main :: IO ()
main = do
    print (k (V.fromList [ stringHexToNumeric "1918"
             , stringHexToNumeric "1110"
             , stringHexToNumeric "0908"
             , stringHexToNumeric "0100"]) 22)
