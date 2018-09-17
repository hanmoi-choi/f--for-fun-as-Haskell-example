module UnderstandingMapandApply where

-- Page 6

mapOption :: (a -> b) -> Maybe a -> Maybe b
mapOption f maybe = case maybe of
  Nothing -> Nothing
  Just v -> Just $ f v

mapList :: (a -> b) -> [a] -> [b]
mapList f xs = case xs of
  [] -> []
  (h:tail) -> f h : mapList f tail

add1 :: Int -> Int
add1 x = x + 1

-- fmap = <$>

add1IfSomething :: Maybe Int -> Maybe Int
add1IfSomething = fmap add1

add1ToEachElement :: [Int] -> [Int]
add1ToEachElement = fmap add1
