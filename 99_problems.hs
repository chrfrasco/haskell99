-- Functions from haskell wiki 99 problems
-- Author: Christian Scott (christianfscott@gmail.com)
-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

import Data.List

-- Question 1

myLast :: [a] -> a
myLast [] = error "No last for empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

-- Question 2

myButLast :: [a] -> a
myButLast [] = error "No second to last for empty list"
myButLast [x] = error "No second to last for list of length one"
myButLast [x, y] = x
myButLast (x:xs) = myButLast xs

-- Question 3

elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt [] _     = error "index out of bounds"
elementAt (_:xs) n
	| n < 1        = error "index out of bounds"
	| otherwise    = elementAt xs (n-1)

-- Question 4

myLength :: [a] -> Int
myLength xs = myLengthRec xs 0
	where
		myLengthRec [] n     = n
		myLengthRec (x:xs) n = myLengthRec xs (n+1)
		
-- Question 5

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Question 6

isPalindrome :: Eq a => [a] -> Bool
isPalindrome []  = True
isPalindrome [x] = True
isPalindrome xs  = (head xs) == (last xs) && (isPalindrome $ tail $ init xs)

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' xs = xs == reverse xs

-- Question 7

data Nested a = Elem a | List [Nested a]

flatten :: (Nested a) -> [a]
flatten (Elem x)  = [x]
flatten (List xs) = concatMap flatten xs

-- Question 8

compress :: Eq a => [a] -> [a]
compress [] = []
compress myseq = compressHelper [] myseq
	where
		compressHelper prev []   = prev
		compressHelper [] (x:xs) = compressHelper [x] xs
		compressHelper prev (x:xs)
			| (last prev) == x   = compressHelper prev xs
			| otherwise          = compressHelper (prev ++ [x]) xs

            
compress' :: Eq a => [a] -> [a]
compress' = map head . group

compress'' :: Eq a => [a] -> [a]
compress'' (x:ys@(y:_))
    | x == y    = compress'' ys
    | otherwise = x : compress'' ys
compress'' [x] = [x]
	
-- Question 9

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = let (first,rest) = span (==x) xs
              in (x:first) : pack rest

-- Question 10
			  
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\xs -> (length xs, head xs)) (pack xs)

-- Question 11

data ListItem a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified = map encodeHelper . encode
    where 
        encodeHelper (1, x) = Single x
        encodeHelper (n, x) = Multiple n x
		
-- Question 12

decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
	where
		decodeHelper (Single a)     = [a]
		decodeHelper (Multiple n a) = replicate n a













