import Data.List
import Language.Haskell.TH (clause)

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
pack (x : xs) =
  let (first, rest) = span (== x) xs
   in (x : first) : pack rest

encode :: (Eq a) => [a] -> [(Int, a)]
encode x =
  let packed = pack x
   in map (\(y : ys) -> (length ys + 1, y)) packed

-- Their version
encode' :: (Eq a) => [a] -> [(Int, a)]
encode' x = map (\y -> (length y, head y)) (group x)

data ListItem a = Single a | Multiple Int a
  deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified x = map toListMap $ encode' x
 where
  -- pattern match instead of if
  -- toListMap (1, n) = Single n
  -- toListMap (x, x) = Multiple x n
  toListMap (lgt, item) =
    if lgt == 1
      then Single item
      else Multiple lgt item

decodeModified :: [ListItem a] -> [a]
-- can use concatMap with a where clause
-- decodeModified = concatMap decodeHelper
-- where
-- decodeModified (Single x) = [x]
-- decodeModified (Multiple n x) = replicate n x
decodeModified (Single x : xs) = x : decodeModified xs
decodeModified (Multiple lgt item : xs) = replicate lgt item ++ decodeModified xs

-- Their solution (didn't get the instructions right cos I'm dumb dumb)
-- `foldr func acc y` => func y acc (cos why tf not)
-- [1,2,2,3]
-- acc = []
-- 3 [] => [(1, 3)]
-- 2 [(1, 3)] => [(1, 2), (1, 3)]
-- 2 [(1, 2), (1, 3)] => [(2, 2), (1, 3)]
-- 1 [(2, 2), (1, 3)] => [(1, 1), (2, 2), (1, 3)]
encode2' :: Eq a => [a] -> [(Int, a)]
encode2' = foldr helper []
 where
  helper x [] = [(1, x)]
  helper x (y@(a, b) : ys)
    | x == b = (1 + a, x) : ys
    | otherwise = (1, x) : y : ys

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = map encodeHelper . encode2'
 where
  encodeHelper (1, x) = Single x
  encodeHelper (n, x) = Multiple n x
