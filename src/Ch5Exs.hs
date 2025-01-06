module Ch5Exs where

squareSum :: Int -> Int
squareSum n = sum [x^2 | x <- [1 .. n]]

replicate2 :: Int -> a -> [a]
replicate2 n c = [c | _ <- [1 .. n]]

pyths :: Int -> [(Int, Int, Int)]
pyths lim = [(x, y, z) | x <- [1 .. lim], y <- [1 .. lim], z <- [1 .. lim], x^2 + y^2 == z^2]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects lim = [x | x <- [1 .. lim], sum (init (factors x)) == x]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (kp, v) <- t, kp == k]

positions2 :: Eq a => a -> [a] -> [Int]
positions2 x xs = find x (zip xs [0 .. length xs-1])

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]

{- 
[(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]] 
produces the same output as:
concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]
both produce:
[(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6)]
the first is one comprehensiom with two generators, the other is two nested
comprehensions, each with one generator
-}
   
