{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Day5 where 

import Data.List.Split
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Base
import Debug.Trace

(>>>) = flip (.)

data Mode = Position | Immediate deriving (Eq, Show)
data Command = Command Int Mode Mode Mode

createMArray :: [Int] -> MyST s
createMArray xs = newListArray (0, (length xs) - 1) xs

extractDigit :: Int -> Int -> Int
extractDigit n pos = (n `mod` (10 ^ (pos + 1))) `div` (10 ^ pos)

extractMode :: Int -> Int -> Mode
extractMode n pos
    | d == 0 = Position
    | d == 1 = Immediate
    where d = extractDigit n pos

parseCommand :: Int -> Command
parseCommand i = Command (i `mod` 100) (extractMode i 2) (extractMode i 3) (extractMode i 4)

type MyST s = ST s (STUArray s Int Int)

readA arr p mode = do
    x <- readArray arr p
    if mode == Immediate then return x else readArray arr x

writeA arr p mode val = do
    p2 <- if mode == Position then readArray arr p else return p
    writeArray arr p2 val

myTrace x = (trace (show x) x)

data State = State Int [Int] [Int] deriving (Show)

processPos arr (State pos input output) = do
    code :: Int <- readArray arr pos

    let (Command op m1 m2 m3) = parseCommand code

    pos2 <- do
            case op of
                1 -> (performOp (+) m1 m2 m3) >> nextPost 4 pos
                2 -> (performOp (*) m1 m2 m3) >> nextPost 4 pos
                99 -> return (-1)
                3 -> (writeA arr (pos + 1) m1 (head input)) >> nextPost 2 pos  -- Input
                4 -> nextPost 2 pos
                5 -> do  -- jump-if-true
                    val <- readA arr (pos + 1) m1
                    if val /= 0 then readA arr (pos + 2) m2 else nextPost 3 pos
                6 -> do  -- jump-if-false
                    val <- readA arr (pos + 1) m1
                    if val == 0 then readA arr (pos + 2) m2 else nextPost 3 pos
                7 -> (performOp (\x y -> if x < y then 1 else 0) m1 m2 m3) >> nextPost 4 pos
                8 -> (performOp (\x y -> if x == y then 1 else 0) m1 m2 m3) >> nextPost 4 pos

    let input2 = if op == 3 then tail input else input
    output2 <- do
        if op == 4 then do
            val <- readA arr (pos + 1) m1
            return (val : output)
        else return output

    return (State pos2 input2 output2)

        where
            nextPost size pos = do
                numElem <- getNumElements arr
                let nextP = pos + size
                return $ if nextP > (numElem - 1) then (-1) else nextP

            performOp f m1 m2 m3 = do
                x <- readA arr (pos + 1) m1
                y <- readA arr (pos + 2) m2
                let val = f x y 
                writeA arr (pos + 3) m3 val

runProgram xs input = do
    arr <- createMArray xs
    s <- go arr (State 0 input [])
    return s

    where go arr s@(State pos input ouput)
            | pos < 0 = return s
            | otherwise = do
                        s2 <- processPos arr s
                        go arr s2

resolve xs =
    let sol1 = (runST $ runProgram xs [1]) 
        sol2 = (runST $ runProgram xs [5])
    in (sol1, sol2)

main :: IO ()
main = interact $ splitOn "," >>> fmap read >>> resolve >>> show

