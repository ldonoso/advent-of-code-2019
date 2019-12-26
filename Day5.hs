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

myUntil p f i = do
    if p i
        then return i
        else do
            i' <- f i
            myUntil p f i'

data Mode = Position | Immediate deriving (Eq, Show)
data Command = Command Int Mode Mode Mode

extractDigit :: Int -> Int -> Int
extractDigit n pos = (n `mod` (10 ^ (pos + 1))) `div` (10 ^ pos)

extractMode :: Int -> Int -> Mode
extractMode n pos
    | d == 0 = Position
    | d == 1 = Immediate
    where d = extractDigit n pos

parseCommand :: Int -> Command
parseCommand i = Command (i `mod` 100) (extractMode i 2) (extractMode i 3) (extractMode i 4)

-- operate:: Int -> Int -> Int
type MyST s = ST s (STUArray s Int Int)

readA arr p mode = do
    x <- readArray arr p
    if mode == Immediate then return x else readArray arr x

writeA arr p mode val = do
    p2 <- if mode == Position then readArray arr p else return p
    writeArray arr p2 val

myTrace x = (trace (show x) x)

processPos arr input p = do
    code :: Int <- readArray arr p
    let (Command op m1 m2 m3) = parseCommand code
    nextP <- do
            case op of
                1 -> (performOp (+) m1 m2 m3) >> nextPost 4 p
                2 -> (performOp (*) m1 m2 m3) >> nextPost 4 p
                99 -> return (-1)
                3 -> (writeA arr (p + 1) m1 (head input)) >> nextPost 2 p  -- Input
                4 -> do -- Output
                        out <- readA arr (p + 1) m1
                        (trace (show out) (nextPost 2 p))  -- Output
                5 -> do  -- jump-if-true
                    val <- readA arr (p + 1) m1
                    if val /= 0 then readA arr (p + 2) m2 else nextPost 3 p
                6 -> do  -- jump-if-false
                    val <- readA arr (p + 1) m1
                    if val == 0 then readA arr (p + 2) m2 else nextPost 3 p
                7 -> (performOp (\x y -> if x < y then 1 else 0) m1 m2 m3) >> nextPost 4 p
                8 -> (performOp (\x y -> if x == y then 1 else 0) m1 m2 m3) >> nextPost 4 p

    return (op, nextP)

        where
            nextPost size p = do
                numElem <- getNumElements arr
                let nextP = p + size
                return $ if nextP > (numElem - 1) then (-1) else nextP

            performOp f m1 m2 m3 = do
                x <- readA arr (p + 1) m1
                y <- readA arr (p + 2) m2
                let val = f x y 
                writeA arr (p + 3) m3 val

createMArray :: [Int] -> MyST s
createMArray xs = newListArray (0, (length xs) - 1) xs

runProgram xs input = do
    arr <- createMArray xs
    pos <- go arr 0 input

    return arr

    where go arr pos input
            | pos < 0 = return pos
            | otherwise = do
                        (op, pos2) <- processPos arr input pos
                        go arr pos2 (if op == 3 then tail input else input)

resolve xs =
    let sol1 = (runSTUArray $ runProgram xs [1]) ! 0
        sol2 = (runSTUArray $ runProgram xs [5]) ! 0
    in (sol1, sol2)

main :: IO ()
main = interact $ splitOn "," >>> fmap read >>> resolve >>> show

