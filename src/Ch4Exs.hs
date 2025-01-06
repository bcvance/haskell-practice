module Ch4Exs where

halve :: [a] -> ([a],[a])
halve xs = 
       if mod (length xs) 2 == 0 
              then (take mid xs, drop mid xs) 
       else error "Error: the input list must have an even number of items."
       where mid = length xs `div` 2


safetail1 :: [a] -> [a]
safetail1 xs =
       if null xs
              then []
       else tail xs

safetail2 :: [a] -> [a]
safetail2 xs  | null xs = []
              | otherwise = tail xs

safetail3 :: [a] -> [a]
safetail3 (_:xs)     = xs
safetail3 []         = []

(||) ::  Bool -> Bool -> Bool
False || b    = b
True || _     = True

testAnd :: Bool -> Bool -> Bool
testAnd bool1 bool2 = 
       if bool1
              then if bool2
                     then True
              else False
       else False

testAnd2 :: Bool -> Bool -> Bool
testAnd2 bool1 bool2 =
       if bool1 
              then bool2
       else False

mult :: Num a => a -> a -> a -> a
mult = \x -> (\y -> (\z -> x * y * z))