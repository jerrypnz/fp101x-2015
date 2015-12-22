import Data.List
import Data.Char
import Hugs.IOExts (unsafeCoerce)

data Nat = Zero | Succ Nat deriving Show

natToInteger (Succ n) = 1 + natToInteger n
natToInteger Zero = 0

natToInteger1 = head . m
  where m Zero = [0]
        m (Succ n) = [sum [x | x <- (1 : m n)]]

natToInteger2 :: Nat -> Integer
natToInteger2 = \n -> genericLength [c | c <- show n, c == 'S']

-- Compilation error: a -> Int does not match Nat -> Integer
-- natToInteger3 :: Nat -> Integer
-- natToInteger3 = \n -> length [c | c <- show n, c == 'S']

integerToNat (n+1) = Succ (integerToNat n)
integerToNat 0 = Zero

-- integerToNat n
--  = product [(unsafeCoerce c) :: Integer | c <- show n]

integerToNat1 (n+1) = let m = integerToNat1 n in Succ m
integerToNat1 0 = Zero

-- integerToNat2 = head . m
--   where {
--     ; m 0 = [0]
--     ; m (n + 1) = [sum [x | x <- (1 : m n)]]
--     }

-- Doesn't compile
-- integerToNat3 :: Integer -> Nat
-- integerToNat3 = \n -> genericLength [c | c <- show n, isDigit c]

add Zero n = n
add (Succ m) n = Succ (add n m)

add1 Zero n = n
add1 (Succ m) n = Succ (add m n)

add2 n Zero = n
add2 n (Succ m) = Succ (add n m)

mult m Zero = Zero
mult m (Succ n) = add m (mult m n)


data Tree = Leaf Integer | Node Tree Integer Tree

occurs m (Leaf n) = m == n
occurs m (Node l n r)
  = case compare m n of
        LT -> occurs m l
        EQ -> True
        GT -> occurs m r

-- Getting tired of typing all the code manually from this point. So going to solve the problems by reasoning about the program ...
-- ;-)
