swap (x, y) = (y, x)

double x = x * 2

plaindrome xs = reverse xs == xs

twice f x = f (f x)

revTake xs = take 3 (reverse xs)

-- Doesn't need to have an 'otherwise' here.
sneaky x | x > 0   = x
         | otherwise = -x


-- A small exercise
zipMe :: [a] -> [b] -> [(a, b)]
zipMe (x:xs) (y:ys) = (x, y):(zipMe xs ys)
zipMe _      _      = []


const x = \_ -> x

e9 [x, y] = (x, True)

e10 (x, y) = [x, y]

-- e13 :: Int -> Int -> Int
e13 x y = x + y * y

-- For fun: defining reduce with recursion
-- reduce' :: (a -> b -> a) -> a -> [b] -> a
reduce' _ n [] = n
reduce' f n (x:xs) = reduce' f (f n x) xs

ones = 1 : ones
