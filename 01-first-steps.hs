-- nth
-- [1, 2, 3, 4, 5] !! 2

-- init/last, like head/tail, but from the end of the list.

double x = x * x

-- proper indentation
foo = x + y
      where
        x = 3
        y = 4

-- type inference fun
sumOfAll xss = sum (map sum xss)

-- type of [] (a really good question of which I dunno the answer): [a]

-- Main> :t []
-- [] :: [a]
-- Main> :t [length]
-- [length] :: [[a] -> Int]
-- Main> :t [head]
-- [head] :: [[a] -> a]
-- Main> :t [length, head]
-- [length,head] :: [[Int] -> Int]

-- This is very interesting:
-- Main> (head (tail [length, head])) "Hello"
-- ERROR - Type error in application
-- *** Expression     : head (tail [length,head]) "Hello"
-- *** Term           : tail [length,head]
-- *** Type           : [[Int] -> Int]
-- *** Does not match : [[Char] -> Int]

n = a `div` length xs
    where a = 10
          xs = [1, 2, 3, 4, 5]

-- The following are all wrong
-- n1 = a `div` length xs
-- where
--   a = 10
--   xs = [1, 2, 3, 4, 5]

xs = [1, 2, 3]
