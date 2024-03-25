import Data.Ord

split :: [a] -> ([a], [a])
split xs = (take n xs, drop n xs)
    where n = length xs `div` 2

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _ [] ys  = ys
mergeBy _ xs []  = xs
mergeBy f (x:xs) (y:ys) = 
  case f x y of
    GT -> y : mergeBy f (x:xs) ys
    _  -> x : mergeBy f xs (y:ys)

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy f xs
  | length xs < 2 = xs
  | otherwise     = let (l,r) = split xs
                    in uncurry (mergeBy f) (sortBy f l, sortBy f r)

sort :: Ord a => [a] -> [a]
sort = sortBy compare
