{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module Day9 where

import Data.List.Split
import Control.Monad.ST
import qualified Day5 as D5
import qualified Data.List as L
import Data.Array.ST
import Debug.Trace (traceShowId)
import Data.Int

(>>>) = flip (.)

resolve1 prog = runST $ D5.runProgram prog [1]
resolve2 prog = runST $ D5.runProgram prog [2]

main :: IO ()
main = do
    contents <- getContents
    let prog = fmap (read @Integer) (splitOn "," contents)
    print (resolve1 prog)
    print (resolve2 prog)

