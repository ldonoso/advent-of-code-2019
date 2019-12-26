
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

(>>>) = flip (.)

resolve' pred s e = length $ filter pred [s..e]

isValid :: Int -> Bool
isValid x = isOrdered digits && twoEquals digits
    where 
        digits = show x

isOrdered xs = and $ fmap (\(l, r) -> l <= r) (zip xs (drop 1 xs))

twoEquals :: Eq a => [a] -> Bool
twoEquals (x : []) = False
twoEquals (x1 : x2 : xs)
    | x1 == x2 = True
    | otherwise = twoEquals (x2 : xs)

twoEquals2 :: Eq a => [a] -> Bool
twoEquals2 [] = False
twoEquals2 (x : []) = False
twoEquals2 (x1 : x2 : [])
    | x1 == x2 = True
    | otherwise = False
twoEquals2 (x1 : x2 : x3 : xs)
    | (x1 == x2) && (x2 /= x3) = True
    | (x1 == x2) && (x2 == x3) = twoEquals2 (dropWhile (== x3) xs)
    | (x1 /= x2) = twoEquals2 (x2 : x3 : xs)

resolve :: (Int, Int)
resolve =
    let
        filtered1 = filter isValid [240298..784956]
        sol1 = length filtered1
        filtered2 = filter (twoEquals2 . show) filtered1
        sol2 = length filtered2
    in (sol1, sol2)

main :: IO ()
main = putStr . show $ resolve




