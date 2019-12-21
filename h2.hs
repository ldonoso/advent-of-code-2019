{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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


-- operate:: Int -> Int -> Int
type MyST s = ST s (STUArray s Int Int)

processPos arr p = do
    command <- readArray arr p
    nextP <- do
                case command of
                    1 -> (performOp (+) p) >> nextPost p
                    2 -> (performOp (*) p) >> nextPost p
                    99 -> return (-1)
    return nextP
    where
        nextPost p = do
            numElem <- getNumElements arr
            let nextP = p + 4
            return $ if nextP > (numElem - 1) then (-1) else nextP

        performOp op p = do
            x <- readArray arr (p + 1)
            y <- readArray arr (p + 2)
            dest <- readArray arr (p + 3)
            r <- op <$> (readArray arr x) <*> (readArray arr y)
            writeArray arr dest r


setNounVerb arr n v = do
    writeArray arr 1 n
    writeArray arr 2 v


createMArray :: [Int] -> MyST s
createMArray xs = newListArray (0, (length xs) - 1) xs


runProgram :: [Int] -> Int -> Int -> MyST s
runProgram xs noun verb = do
    arr <- createMArray xs
    setNounVerb arr noun verb
    poss <- myUntil (< 0) (processPos arr) 0
    return arr

resolve1 xs n v = (runSTUArray $ runProgram xs n v) ! 0

nounVerbCom :: [(Int, Int)]
nounVerbCom = [(i,j) | i <- [0..99], j <- [0..99]]


resolve2 :: [Int] -> (Int, Int)
resolve2 xs = head $ dropWhile (not . isSol) nounVerbCom
    where isSol (n, v) = (resolve1 xs n v) == 19690720

resolve xs =
    let sol1 = resolve1 xs 12 2
        (n, v) = resolve2 xs
        sol2 = 100 * n + v
    in (sol1, sol2)

main :: IO ()
main = interact $ splitOn "," >>> fmap read >>> resolve >>> show

