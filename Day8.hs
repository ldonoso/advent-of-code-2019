{-# LANGUAGE ScopedTypeVariables #-}
module Day8 where

import Data.List.Split (splitOn, chunksOf)
import Data.List
import Debug.Trace
import Data.Char


data Image = Image { wide :: Int, tall :: Int }


numberOf :: Int -> [Int] -> Int
numberOf n = length . filter ((==) n)


resolve :: Image -> [Int] -> Int
resolve image xs = (numberOf 1 layer) * (numberOf 2 layer)
    where
        layers = chunksOf (wide image * tall image) xs
        layer = minimumBy compareLayers layers
        compareLayers a b = compare (numberOf 0 a) (numberOf 0 b)
         

main :: IO ()
main = do
    contents :: String <- getContents
    let xs :: [Int] = fmap digitToInt $ takeWhile isDigit contents
    print $ resolve (Image 25 6) xs
