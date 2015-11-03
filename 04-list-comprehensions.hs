import Data.Char


pyths n
  = [(x,y,z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n],
     x ^ 2 + y ^ 2 == z ^ 2]

factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects n = [x | x <- [1 .. n], sum (init (factors x)) == x]

find k t = [v | (k', v) <- t, k == k']

positions x xs = find x (zip xs [0 .. n])
  where n = length xs -1

scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]

scalarproduct' xs ys = sum $ zipWith (*) xs ys

-- Caesar cipher
let2int b c = ord c - ord b
int2let b n = chr (ord b + n)

shift n c
  | isLower c = int2let 'a' ((let2int 'a' c + n) `mod` 26)
  | isUpper c = int2let 'A' ((let2int 'A' c + n) `mod` 26)
  | otherwise = c

encode n xs = [shift n x | x <- xs]

pos_numbers = 1 : [x + 1 | x <- pos_numbers]

riffle xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]

divides x y = x `mod` y == 0

divisors n = [x | x <- [1..n], n `divides` x]
