import Prelude hiding (takeWhile, dropWhile, map, filter)

all1 p xs = and (map p xs)
all2 p = and . map p
all3 p = not . any (not . p)
all4 p xs = foldl (&&) True (map p xs)
all5 p = foldr (&&) True . map p

any1 p = or . map p
any2 p xs = length (filter p xs) > 0
any3 p = not . null . dropWhile (not . p)
any4 p xs = not (all (\x -> not (p x)) xs)
any5 p xs = foldr (\x acc -> (p x) || acc) False xs

takeWhile _ [] = []
takeWhile p (x:xs)
  |p x = x : takeWhile p xs
  |otherwise = []

dropWhile _ [] = []
dropWhile p (x:xs)
  | p x = dropWhile p xs
  | otherwise = x:xs

-- The order is wrong
-- dropWhile p = foldl add []
--  where add [] x = if p x then [] else [x]
--        add acc x = x : acc

map f = foldl (\ xs x -> xs ++ [f x]) []

filter p = foldr (\x xs -> if p x then x : xs else xs) []

-- The following doesn't compile because the position of the function parameters needs to be swapped
-- filter' p = foldl (\x xs -> if p x then xs ++ [x] else xs) []
filter' p = foldl (\xs x -> if p x then xs ++ [x] else xs) []

dec2int = foldl (\ x y -> 10 * x + y) 0

foo (x, y) = x + y

mycurry :: ((a, b) -> c) -> a -> b -> c
mycurry f a b = f (a, b)

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

type Bit = Int
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 = unfold null (take 8) (drop 8)

map' f = unfold null (f . head) tail

iterate' f = unfold (const False) id f
