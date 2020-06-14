module Day10 where

import Control.Monad
import Control.Arrow((>>>))
import Data.Function((&))
import qualified Data.Set as S
import qualified Data.List as L
import Data.Ratio

data Coord = Coord Int Int deriving (Show, Eq, Ord)
data Polar = Polar {
                polarAngle :: Float,
                polarLen :: Float
             }

strToCoord :: String -> [Coord]
strToCoord s = do
    (l, y) <- zip (lines s) [0, 1..]
    (c, x) <- zip l [0, 1..]
    guard (c == '#')
    return (Coord x y)

coordToPool :: Coord -> Polar
coordToPool (Coord x y) =
    Polar angle len
    where
        x' = fromIntegral x
        y' = fromIntegral y
        angle = atan ( y' / x')
        len = sqrt (x' ^ 2 + y' ^ 2)
            

normalize :: Coord -> Coord
normalize (Coord x y) =
    if d /= 0
        then Coord (div x d) (div y d)
        else Coord x y
    where d = gcd x y

relocate :: Coord -> Coord -> Coord
relocate (Coord xC yC) (Coord x y) = Coord (x - xC) (y - yC)

getNAst :: [Coord] -> Coord -> Int
getNAst coords center =
    fmap (relocate center) coords &
    fmap normalize &
    S.fromList &
    S.size &
    (subtract 1)

solve :: String -> String
solve s = 
    let coords = strToCoord s
        nAst = fmap (getNAst coords) coords
        nAstMax = L.maximum nAst
    in show nAstMax

maximum' :: (Ord b) => (a -> b) -> [a] -> a
maximum' f (x:xs) = fst $ L.foldl' f' (x, f x) xs
    where f' (y, fy) x =
            if fx  > fy then (x, fx) else (y, fy)
            where fx = f x 

getCenter :: [Coord] -> Coord
getCenter coords =
    fst $ maximum' snd (zip coords nAst)
    where nAst = fmap (getNAst coords) coords

solve2 :: String -> String
solve2 s = 
    let coords = strToCoord s
        center = getCenter coords
    in show center

main = interact solve
