import Prelude hiding ((^), (!!), and, concat, replicate, elem)

-- m ^ 0 = 1
-- m ^ n = m * m ^ (n - 1)

m ^ 0 = 1
m ^ n = m * (^) m (n - 1)

-- and [] = True
-- and (b : bs) =  b && and bs

-- and [] = True
-- and (b : bs)
--   | b = and bs
--   | otherwise = False

-- and [] = True
-- and (b : bs)
--   | b == False = False
--   | otherwise = and bs

and [] = True
and (b : bs) =  and bs && b

concat [] = []
concat (xs : xss) = xs ++ concat xss

replicate 0 _ = []
replicate n x = x : replicate (n - 1) x

(x : _) !! 0 = x
(_ : xs) !! n = xs !! (n - 1)

elem _ [] = False
elem x (y : ys)
  | x == y = True
  | otherwise = elem x ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
  where (left, right) = halve xs
