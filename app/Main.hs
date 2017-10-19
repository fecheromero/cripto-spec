module Main where

import Crypto1

main :: IO ()
main = do
    print $ (k [stringHexToNumeric "1918", stringHexToNumeric "1110", stringHexToNumeric "0908", stringHexToNumeric "0100"] 16 :: Int)
