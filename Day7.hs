{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Day7 where

import Data.List.Split
import Control.Monad.ST
import qualified Day5 as D5
import qualified Data.List as L
import Data.Array.ST
import Debug.Trace (traceShowId)

(>>>) = flip (.)

comb :: (Eq a) => [a] -> [[a]]
comb [] = []
comb (x : []) = [[x]]
comb xs = concat $ fmap go xs
    where go x = (x :) <$> comb (L.delete x xs)

runProgram arr posIni input = do
    s <- go arr (D5.State posIni input [])
    return s

    where go arr s@(D5.State pos input output)
            | pos < 0 = return s
            | not (L.null output) = return s  -- Stop at first output
            | otherwise = do
                        s2 <- D5.processPos arr s
                        go arr s2

runProgram2 phases xs = do
    arr0 <- D5.createMArray xs
    (D5.State pos0 i0 o0) <- runProgram arr0 0 [phases !! 0, 0]

    arr1 <- D5.createMArray xs
    (D5.State pos1 i1 o1) <- runProgram arr1 0 [phases !! 1, head o0]

    arr2 <- D5.createMArray xs
    (D5.State pos2 i2 o2) <- runProgram arr2 0 [phases !! 2, head o1]

    arr3 <- D5.createMArray xs
    (D5.State pos3 i3 o3) <- runProgram arr3 0 [phases !! 3, head o2]

    arr4 <- D5.createMArray xs
    (D5.State pos4 i4 o4) <- runProgram arr4 0 [phases !! 4, head o3]

    return (head o4)


runProgram3 phases xs = do
    arr0 <- D5.createMArray xs
    (D5.State pos0 i0 o0) <- runProgram arr0 0 [(traceShowId phases) !! 0, 0]

    arr1 <- D5.createMArray xs
    (D5.State pos1 i1 o1) <- runProgram arr1 0 [phases !! 1, head o0]

    arr2 <- D5.createMArray xs
    (D5.State pos2 i2 o2) <- runProgram arr2 0 [phases !! 2, head o1]

    arr3 <- D5.createMArray xs
    (D5.State pos3 i3 o3) <- runProgram arr3 0 [phases !! 3, head o2]

    arr4 <- D5.createMArray xs
    (D5.State pos4 i4 o4) <- runProgram arr4 0 [phases !! 4, head o3]

    let arrArr = [arr0, arr1, arr2, arr3, arr4]
    posArr <- D5.createMArray [pos0, pos1, pos2, pos3, pos4]

    res <- go arrArr posArr (cycle [0, 1, 2, 3, 4]) (head o4)
    return res

    where
        go :: [STUArray s Int Int] -> STUArray s Int Int -> [Int] -> Int -> ST s Int
        go arrArr posArr (ix : ixs) signal = do
            let arr = arrArr !! ix 
            pos <- readArray posArr ix
            (D5.State pos i o) <- runProgram arr pos [signal]
            writeArray posArr ix pos
            case o of
                [] -> return signal
                [x] -> go arrArr posArr ixs x


resolve1 prog = L.maximum outputs
    where
        outputs = fmap (\phases -> runST $ runProgram2 phases prog) (comb [0, 1, 2, 3, 4])


resolve2 prog = L.maximum outputs
    where
        outputs = fmap (\phases -> runST $ runProgram3 phases prog) (comb [5, 6, 7,8, 9])


main :: IO ()
main = do
    text <- getContents
    let prog = fmap read (splitOn "," text)
    print (resolve1 prog)
    print (resolve2 prog)

