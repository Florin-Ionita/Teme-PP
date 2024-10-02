-- instance Functor Maybe where
--   fmap f x = 
--     case x of
--        Just v -> Just (f v)
--        Nothing -> Nothing
 
-- instance Monad Maybe where
--   return = Just
--   mx >>= f = 
--       case mx of 
--         Just v -> f v
--         Nothing -> Nothing
-- use fmap
add5 :: Maybe Int -> Maybe Int
add5 mx = fmap (+5) mx
-- add5 (Just 5)
add ma mb = do
    a <- ma
    b <- mb
    return (a + b)
-- add (Just 5) (Just 5)
sub ma mb = do
    a <- ma
    b <- mb
    return (a - b)
mult ma mb = do
    a <- ma
    b <- mb
    return (a * b)
program = putStrLn "Hello World"


fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

myFunc :: String -> Int
myFunc s = fib (read s)

func :: [Int] -> [Int]
func [] = []
-- Daca y e mai mic ca x sau egal il pui in stanga altfel in dreapta
func (x:xs) = func [y | y <- xs, y <= x] ++ [x] ++ func [y | y <- xs, y > x]

readVect :: Int -> String -> [Int]
readVect n s = fmap read (words s)

sortVect :: Int -> String -> [Int]
sortVect n s = func (readVect n s)

collatz :: Int -> [Int]
collatz 1 = [1]
collatz n
    | n `mod` 2 == 0 = n : collatz (n `div` 2)
    | otherwise = n : collatz (3 * n + 1)
