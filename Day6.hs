{-# LANGUAGE ScopedTypeVariables #-}

import Data.MultiMap as MM
import Data.Map as M
import Data.List.Split
import Data.Maybe
import Debug.Trace
import Data.List as L

(>>>) = flip (.)
myTrace x = trace (show x) x

resolve1 :: [(String, String)] -> Int
resolve1 xs = go 0 "COM"
            where
                mm = MM.fromList xs
                go depth x = depth + (sum $ fmap (go (depth + 1)) (MM.lookup x mm))


resolve2 :: [(String, String)] -> Maybe Int
resolve2 xs = go 0 "YOU" "YOU"
            where
                m = M.fromList (fmap (\(x ,y) -> (y, x)) xs)
                mm = MM.fromList xs
                go dist origin x 
                    | x == "SAN" = Just dist
                    | L.null cands' = Nothing
                    | otherwise = if L.null dists then Nothing else head dists
                    where
                        children = MM.lookup x mm
                        parent = M.lookup x m
                        cands :: [String] = if isNothing parent then children else (fromJust parent) : children
                        cands' = L.delete origin cands
                        dists = L.filter isJust $ fmap (go (dist + 1) x) cands'


resolve xs = (resolve1 xs, (subtract 2) <$> (resolve2 xs))


main :: IO ()
main = interact $
        lines >>> fmap (splitOn ")") >>> fmap (\(x : y : xs) -> (x, y))
            >>> resolve >>> show
