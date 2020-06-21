module Day10 where

import Control.Monad
import Control.Arrow ((>>>))
import Data.Function ((&))
import qualified Data.Set as S
import qualified Data.List as L
import Data.Ratio
import Data.List
import Debug.Trace
import Data.Ord (comparing)
import Data.Function (on)
import Data.Maybe (catMaybes)

myTrace x = trace (show x) x

data Coord = Coord Int Int deriving (Show, Eq, Ord)
data Polar = Polar {
                polarAngle :: Double,
                polarLen :: Double
             } deriving (Show)

strToCoord :: String -> [Coord]
strToCoord s = do
    (l, y) <- zip (lines s) [0, 1..]
    (c, x) <- zip l [0, 1..]
    guard (c == '#')
    return (Coord x y)

coordToPolar :: Coord -> Polar
coordToPolar (Coord x y) = Polar angle len
    where
        x' = fromIntegral x
        y' = fromIntegral y
        angle = getAngle x' y'
        len = sqrt (x' ^ 2 + y' ^ 2)

getAngle x y = turnClockwise . rot90CounterClock $ a2
    where
        a = atan2 y x
        a2 = if a < 0 then a + 2 * pi else a 

rot90CounterClock a = a''
    where
        a' = a - (pi / 2)
        a'' = if a' < 0 then a' + 2 * pi else a'

turnClockwise a = if a > 0 then 2 * pi - a else a

relocate :: Coord -> Coord -> Coord
relocate (Coord xC yC) (Coord x y) = Coord (x - xC) (yC - y)

getNAst2 :: [Coord] -> Coord -> Int
getNAst2 coords center =
    fmap (relocate center) (filter ((/=) center) coords) &
    fmap coordToPolar &
    fmap polarAngle &
    S.fromList &
    S.size

getTargetList :: Coord -> [Coord] -> [[(Polar, Coord)]]
getTargetList center coords =
    let 
        coords' = filter ((/=) center) coords
        polars = fmap (relocate center) coords' & fmap coordToPolar
        polarsSorted =
            sortBy (comparing $ polarAngle . fst) (zip polars coords') &
            groupBy ((==) `on` polarAngle . fst) &
            fmap (sortBy (comparing (polarLen . fst)))
    in polarsSorted

solve :: String -> String
solve s = 
    let coords = strToCoord s

        nAst = fmap (getNAst2 coords) coords
        sol1 = L.maximum nAst

        center = getCenter coords
        targetList = mergeLists $ getTargetList center coords
        (_, Coord x y)  = targetList !! 199
        sol2 = x * 100 + y
    in show sol1 <> " " <> show sol2

getCenter :: [Coord] -> Coord
getCenter coords =
    fst $ maximumBy (comparing snd) (zip coords nAst)
    where nAst = fmap (getNAst2 coords) coords

mergeLists :: [[a]] -> [a]
mergeLists xss = concat (go 0)
    where go i =
            case xs of
                [] -> []
                _ -> xs : go (i + 1)
            where 
                xs = catMaybes $ fmap (\xs -> if i < length xs then Just(xs !! i) else Nothing) xss


main = interact solve
