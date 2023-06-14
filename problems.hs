import Data.List
myLast :: [a] -> a
myLast [] = error "Une liste vide n'a pas de dernier element"
myLast [x] = x
myLast (_ : xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "Une liste vide n'a pas d'avant-dernier element"
myButLast [x] = error "La liste n'a qu'un seul element"
myButLast (x : xs) = if length xs == 1 then x else myButLast xs

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Une liste vide n'a pas d'element"
elementAt x idx =
  if length x < idx
    then error "Index trop grand wtf???"
    else x !! (idx - 1)

myLength :: [a] -> Int
myLength [] = 0
-- Next line is not needed as we add one from the start. Keeping it to know what
-- I've done first try
myLength [x] = 1
myLength (_ : xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome x = x == myReverse x -- OR isPalindrome x = x == reverse x

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x : xs)) = flatten x ++ flatten (List xs)

compress :: (Eq a) => [a] -> [a]
compress (x : xs@(y : _)) -- @ reads as, in other words xs as y (head of xs) and _ as the tail
  | x == y = compress xs
  | otherwise = x : compress xs
compress x = x

-- Takeaway: read std (Prelude)
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first, rest) = span (==x) xs
              in (x:first) : pack rest

encode :: (Eq a) => [a] -> [(Int, a)]
encode x = let packed = pack x
           in map (\(y:ys) -> (length ys + 1, y)) packed

-- Their version
encode' :: (Eq a) => [a] -> [(Int, a)]
encode' x = map (\y -> (length y, head y)) (group x)
