{-# LANGUAGE ScopedTypeVariables #-}
module Day8 where

import Data.List.Split (splitOn, chunksOf)
import Data.List
import Debug.Trace
import Data.Char
import Control.Applicative
import Control.Monad


data Image = Image { wide :: Int, tall :: Int }


numberOf :: Int -> [Int] -> Int
numberOf n = length . filter ((==) n)


resolve :: Image -> [Int] -> Int
resolve image xs = (numberOf 1 layer) * (numberOf 2 layer)
    where
        layers = chunksOf (wide image * tall image) xs
        layer = minimumBy compareLayers layers
        compareLayers a b = compare (numberOf 0 a) (numberOf 0 b)


mergePixels :: Int -> Int -> Int
mergePixels 0 _ = 0
mergePixels 1 _ = 1
mergePixels 2 x = x


resolve2 :: Image -> [Int] -> [Int]
resolve2 image xs = getZipList layersFold
    where
        layers = fmap ZipList $ chunksOf (wide image * tall image) xs
        layersFold = foldl1 (\acc x -> mergePixels <$> acc <*> x) layers


printImage :: Image -> [Int] -> IO ()
printImage image xs = forM_ rows (print)
    where
          xsChar = fmap (\x -> if x == 1 then '*' else ' ') xs
          rows = chunksOf (wide image) xsChar
         

main :: IO ()
main = do
    contents <- getContents
    let xs = fmap digitToInt $ takeWhile isDigit contents
    let image = Image 25 6
    print $ resolve image xs
    printImage image $ resolve2 image xs
