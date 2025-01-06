module Ch5Exs where

squareSum :: Int -> Int
squareSum n = sum [x^2 | x <- [1 .. n]]

replicate2 :: Int -> a -> [a]
replicate2 n c = [c | _ <- [1 .. n]]

pyths :: Int -> [(Int, Int, Int)]
pyths lim = [(x, y, z) | x <- [1 .. lim], y <- [1 .. lim], z <- [1 .. lim], x^2 + y^2 == z^2]
