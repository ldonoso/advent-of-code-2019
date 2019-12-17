(>>>) = flip (.)

fuel :: Int -> Int
fuel m = (m `quot` 3) - 2

fuelForFuel :: Int -> [Int]
fuelForFuel f
    | f2 <= 0 = []
    | otherwise = f2 : fuelForFuel f2
    where f2 = fuel(f)

fuel2 :: Int -> Int
fuel2 m = foldr (+) f (fuelForFuel f)
    where f = fuel m

resolve1 :: [Int] -> Int
resolve1 = fmap fuel >>> foldr (+) 0

resolve2 :: [Int] -> Int
resolve2 = fmap fuel2 >>> foldr (+) 0

resolve xs =
    let sol1 = resolve1 xs
        sol2 = resolve2 xs
    in [sol1, sol2]

main :: IO ()
main = interact $
    lines >>> fmap read >>> resolve >>> fmap show >>> unlines
