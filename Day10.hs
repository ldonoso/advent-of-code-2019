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
        angle = atan2 y' x'
        len = sqrt (x' ^ 2 + y' ^ 2)

relocate :: Coord -> Coord -> Coord
relocate (Coord xC yC) (Coord x y) = Coord (x - xC) (yC - y)

getNAst2 :: [Coord] -> Coord -> Int
getNAst2 coords center =
    fmap (relocate center) (filter ((/=) center) coords) &
    fmap coordToPolar &
    fmap polarAngle &
    S.fromList &
    S.size

solve :: String -> String
solve s = 
    let coords = strToCoord s
        nAst = fmap (getNAst2 coords) coords
        nAstMax = L.maximum nAst
    in show nAstMax

getCenter :: [Coord] -> Coord
getCenter coords =
    fst $ maximumBy (comparing snd) (zip coords nAst)
    where nAst = fmap (getNAst2 coords) coords

main = interact solve
