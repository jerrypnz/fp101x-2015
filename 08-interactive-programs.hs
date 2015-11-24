putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = putChar x >> putStr' xs

putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""

putStrLn1 [] = putChar '\n' -- This isn't really needed.
putStrLn1 xs = putStr' xs >> putChar '\n'

putStrLn2 [] = putChar '\n' -- This isn't really needed.
putStrLn2 xs = putStr' xs >>= \ x -> putChar '\n'

putStrLn3 [] = putChar '\n'
putStrLn3 xs = putStr' xs >> putStr' "\n"

-- This is wrong
putStrLn4 [] = putChar '\n'
putStrLn4 xs = putStr' xs >> putStrLn' "\n"

getLine' = get []

get :: String -> IO String
get xs = do x <- getChar
            case x of
              '\n' -> return xs
              _    -> get (xs ++ [x])


interact' f = do input <- getLine'
                 putStrLn' (f input)



--- Some more monad fun

sequence_' :: Monad m => [m a] -> m ()

-- Doesn't compile
-- sequence_' [] = return []
-- sequence_' (m:ms) = m >> \_ -> sequence_' ms

-- Works but gets unresolved overloading for []
--sequence_' [] = return ()
--sequence_' (m:ms) = (foldl (>>) m ms) >> return ()

-- Doesn't compile: Inferred type is not general enough
-- sequence_' ms = foldl (>>) (return ()) ms

-- Works but gets unresolved overloading for []
-- sequence_' [] = return ()
-- sequence_' (m:ms) = m >> sequence_' ms

-- Works but gets unresolved overloading for []
-- sequence_' [] = return ()
-- sequence_' (m:ms) = m >>= \_ -> sequence_' ms

-- Doesn't compile
-- sequence_' ms = foldr (>>=) (return ()) ms

-- Works but gets unresolved overloading for []
sequence_' ms = foldr (>>) (return ()) ms

-- Doesn't compile
-- sequence_' ms = foldr (>>) (return []) ms


sequence' :: Monad m => [m a] -> m [a]

-- Try writing one myself
sequence' = foldr (\m ms -> do x  <- m
                               xs <- ms
                               return $ x:xs)
                  (return [])

-- Works
-- sequence' [] = return []
-- sequence' (m:ms)
--   = m >>=
--     \ a ->
--       do as <- sequence' ms
--          return (a:as)

-- Doesn't compile, should replace `return ()` with `return []`
-- sequence' ms = foldr func (return ()) ms
--   where func :: (Monad m) => m a -> m [a] -> m [a]
--         func m acc
--           = do x <- m
--                xs <- acc
--                return (x:xs)

-- Doesn't compile
-- sequence' ms = foldr func (return []) ms
--   where func :: (Monad m) => m a -> m [a] -> m [a]
--         func m acc = m : acc

-- Doesn't compile: syntax error
-- sequence' [] = return []
-- sequence' (m:ms) = return (a:as)
--   where a  <- m
--         as <- sequence' ms

-- Doesn't compile: should replace `>>` with `>>=`
-- sequence' [] = return []
-- sequence' (m:ms)
--   = m >>
--       \a ->
--        do as <- sequence' ms
--           return (a:as)

-- Doesn't compile: syntax error
-- sequence' [] = return []
-- sequence' (m:ms) = m >>= \a ->
--     as <- sequence' ms
--     return (a:as)

-- Works
-- sequence' [] = return []
-- sequence' (m:ms)
--   = do a  <- m
--        as <- sequence' ms
--        return (a:as)


mapM' :: Monad m => (a -> m b) -> [a] -> m [b]

-- Try implementing one myself
mapM' f = foldr (\x acc -> do a  <- f x
                              as <- acc
                              return $ a:as)
                (return [])

-- Works
-- mapM' f as = sequence' (map f as)

-- Works
-- mapM' f [] = return []
-- mapM' f (a:as)
--   = f a >>= \b -> mapM' f as >>= \ bs -> return (b:bs)

-- Doesn't compile
-- mapM' f as = sequence_' (map f as)

-- Works
-- mapM' f [] = return []
-- mapM' f (a:as)
--   = do b  <- f a
--        bs <- mapM' f as
--        return (b:bs)

-- Works
-- mapM' f [] = return []
-- mapM' f (a:as)
--   = f a >>=
--       \ b ->
--         do bs <- mapM' f as
--            return (b:bs)

-- The order of the result is wrong
-- mapM' f [] = return []
-- mapM' f (a:as)
--   = f a >>=
--       \ b ->
--         do bs <- mapM' f as
--            return (bs ++ [b])

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]

-- Try implementing one myself
-- filterM' p = foldr (\x acc -> do y  <- p x
--                                  xs <- acc
--                                  return $ if y then (x:xs) else xs)
--                    (return [])

filterM' _ [] = return []
filterM' p (x:xs)
  = do flag <- p x
       ys   <- filterM' p xs
       if flag then return (x:ys) else return ys

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM _ a [] = return a
foldLeftM f a (x:xs) = f a x >>= \v -> foldLeftM f v xs

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM _ b [] = return b
foldRightM f b (x:xs) = foldRightM f b xs >>= f x

liftM :: Monad m => (a -> b) -> m a -> m b

-- works
liftM f m = m >>= \a -> return $ f a

-- Doesn't compile
-- liftM f m = mapM f [m]

-- Could run the monad twice, which is wrong for IO monads (should've tested it with IO rather than Maybe)
--liftM f m = m >>= \a -> m >>= \b -> return (f a)
--liftM f m = m >>= \a -> m >>= \b -> return (f b)
