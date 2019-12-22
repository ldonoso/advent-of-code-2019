{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

import Data.List.Split
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Base
import Debug.Trace
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.List (elemIndex)

(>>>) = flip (.)

type Dir = Char
data Point = Point { x :: Int, y :: Int } deriving (Eq, Ord, Show)
data Command = Command { dir :: Dir, dist :: Int }

movePoint :: Dir -> Point -> Point
movePoint d (Point x y) =
    case d of
        'L' -> Point (x - 1) y
        'R' -> Point (x + 1) y
        'D' -> Point x (y - 1)
        'U' -> Point x (y + 1)

expandCommand :: Command -> Point -> [Point]
expandCommand (Command dir dist) = take dist . tail . iterate (movePoint dir)

parseCommand :: String -> Command
parseCommand s = Command (head s) (read . tail $ s)

genPoints :: [Command] -> [Point]
genPoints = snd . foldl f ((Point 0 0), [(Point 0 0)])
    where f (point, points) command =
            let newPoints = expandCommand command point
            in (last newPoints, points ++ newPoints)

resolve1 :: [[Command]] -> (Int, Int)
resolve1 commandss = (minDist, minSteps)
    where 
        routes = fmap genPoints commandss
        sets = fmap Set.fromList routes
        inters = Set.toList $ Set.delete (Point 0 0) $ foldr1 Set.intersection sets

        minDist = minL dist
            where dist = fmap (\(Point x y) -> abs x + abs y) inters

        minSteps = minL dist
            where dist = fmap getPos inters
                  steps point = fmap (elemIndex' point) routes
                  getPos = sum . steps
    
minL = foldr1 min
elemIndex' a = fromJust . elemIndex a

resolve input =
    let sol1 = resolve1 input
        sol2 = ""
    in (sol1, sol2)

main :: IO ()
main = interact $ readInput >>> resolve >>> show
    where readInput = lines >>> fmap (splitOn "," >>> fmap parseCommand)




