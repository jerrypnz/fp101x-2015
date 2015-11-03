import Prelude hiding ((||))

-- halve1 xs = (take n xs, drop n xs)
--  where n = length xs / 2

halve1 xs = splitAt (length xs `div` 2) xs
halve2 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs

-- halve xs = splitAt (length xs `div` 2) xs
halve = splitAt =<< (`div` 2) . length

halve3 xs = splitAt (div (length xs) 2) xs
halve4 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

safetail1 xs = if null xs then [] else tail xs

safetail2 [] = []
safetail2 (_ : xs) = xs

safetail3 xs
  | null xs = []
  | otherwise = tail xs

safetail4 [] = []
safetail4 xs = tail xs

-- Pattern match failure
-- safetail5 [x] = [x]
-- safetail5 (_ : xs) = xs

safetail6
  = \ xs ->
    case xs of
        [] -> []
        (_ : xs) -> xs

-- False || False = False
-- _ || _ = True

-- Wrong
-- b || c
--  | b == c = True
--  | otherwise = False

-- b || c
--   | b == c =b
--   | otherwise = True

False || False = False
False || True = True
True || False = True
True || True = True
