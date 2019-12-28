{-# LANGUAGE FlexibleContexts #-}

module Day7 where

import Data.List.Split
import Control.Monad.ST
import qualified Day5 as D5
import qualified Data.List as L

(>>>) = flip (.)

comb :: (Eq a) => [a] -> [[a]]
comb [] = []
comb (x : []) = [[x]]
comb xs = concat $ fmap go xs
    where go x = (x :) <$> comb (L.delete x xs)

runProgram xs input = do
    arr <- D5.createMArray xs
    s <- go arr (D5.State 0 input [])
    return s

    where go arr s@(D5.State pos input output)
            | pos < 0 = return s
            | not (L.null output) = return s  -- Stop at first output
            | otherwise = do
                        s2 <- D5.processPos arr s
                        go arr s2

resolveP :: [Int] -> Int -> [Int] -> Int
resolveP [] initial _ = initial
resolveP (p : ps) initial xs =
    resolveP ps (head output) xs
    where
        arr = D5.createMArray xs
        (D5.State pos input output) = runST $ runProgram xs (p : [initial]) 

resolve prog = L.maximum outputs
    where outputs = fmap (\phases -> resolveP phases 0 prog) (comb [0, 1, 2, 3, 4])

main :: IO ()
main = interact $ splitOn "," >>> fmap read >>> resolve >>> show

