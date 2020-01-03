{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Day5 where 

import Data.List.Split
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Base
import Debug.Trace (traceShowId)
import Data.Int

(>>>) = flip (.)

data Mode = Position | Immediate | Relative deriving (Eq, Show)
data Command = Command Integer Mode Mode Mode

createMArray :: [Integer] -> MyST s
createMArray xs = 
    newListArray (0, fromIntegral $ 8 * len - 1) (xs ++ (take (7 * len) [0,0..]))
    where
        len = length xs

extractDigit :: (Integral a) => a -> a -> a
extractDigit n pos = (n `mod` (10 ^ (pos + 1))) `div` (10 ^ pos)

extractMode :: Integer -> Integer -> Mode
extractMode n pos
    | d == 0 = Position
    | d == 1 = Immediate
    | d == 2 = Relative
    where d = extractDigit n pos

parseCommand :: Integer -> Command
parseCommand i = Command (i `mod` 100) (extractMode i 2) (extractMode i 3) (extractMode i 4)

type MyST s = ST s (STArray s Integer Integer)

readA arr p mode relBase = do
    x <- readArray arr p
    case mode of
        Immediate -> return x
        Position -> readArray arr x
        Relative -> readArray arr (x + relBase)

writeA arr p mode relBase val = do
    x <- readArray arr p
    let p2 = case mode of
            -- There is no Immediate mode in write operation
            Position -> x
            Relative -> x + relBase
    writeArray arr p2 val


data State = State Integer [Integer] [Integer] Integer deriving (Show)

processPos arr (State pos input output relBase) = do
    code :: Integer <- readArray arr pos

    let (Command op m1 m2 m3) = parseCommand code

    pos2 <- do
            case op of
                1 -> (performOp (+) m1 m2 m3) >> nextPost 4 pos
                2 -> (performOp (*) m1 m2 m3) >> nextPost 4 pos
                99 -> return (-1)
                3 -> (writeA arr (pos + 1) m1 relBase (head input)) >> nextPost 2 pos  -- Input
                4 -> nextPost 2 pos
                5 -> do  -- jump-if-true
                    val <- readA arr (pos + 1) m1 relBase
                    if val /= 0 then readA arr (pos + 2) m2 relBase else nextPost 3 pos
                6 -> do  -- jump-if-false
                    val <- readA arr (pos + 1) m1 relBase
                    if val == 0 then readA arr (pos + 2) m2 relBase else nextPost 3 pos
                7 -> (performOp (\x y -> if x < y then 1 else 0) m1 m2 m3) >> nextPost 4 pos
                8 -> (performOp (\x y -> if x == y then 1 else 0) m1 m2 m3) >> nextPost 4 pos
                9 -> nextPost 2 pos

    let input2 = if op == 3 then tail input else input

    output2 <- do
        if op == 4 then do
            val <- readA arr (pos + 1) m1 relBase
            return (val : output)
        else return output

    relBase2 <- do
        if op == 9 then do
            val <- (readA arr (pos + 1) m1 relBase)
            return (val + relBase)
        else return relBase

    return (State pos2 input2 output2 relBase2)

        where
            nextPost size pos = do
                numElem <- fmap fromIntegral $ getNumElements arr
                let nextP = pos + size
                return $ if nextP > (numElem - 1) then (-1) else nextP

            performOp f m1 m2 m3 = do
                x <- readA arr (pos + 1) m1 relBase
                y <- readA arr (pos + 2) m2 relBase
                let val = f x y 
                writeA arr (pos + 3) m3 relBase val

runProgram xs input = do
    arr <- createMArray xs
    s <- go arr (State 0 input [] 0)
    return s

    where go arr s@(State pos input ouput relBase)
            | pos < 0 = return s
            | otherwise = do
                        s2 <- processPos arr s
                        go arr s2

resolve xs =
    let sol1 = (runST $ runProgram xs [1]) 
        sol2 = (runST $ runProgram xs [5])
    in (sol1, sol2)

main :: IO ()
main = interact $ splitOn "," >>> fmap (read @Integer) >>> resolve >>> show

