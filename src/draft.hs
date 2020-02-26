import System.Environment
import Prelude


main :: IO()
main = do
    line <- getLine
    if null line
        then return ()
        else do
                putStrLn $ reversal line
                main 




reversal :: String -> String 
reversal [] = []
reversal (x:xs) = reversal xs ++ [x]


-- fib = 0 : 1 : zipWith (+) fib (tail)
-- fib :: [Integer]
-- fib = 0 : 1 : [(+) (last $ init fib) (last fib)]


factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x-1)

cond :: (Ord a, Num a) => a -> IO()
cond a = if a > 10 then print "yes" else print "no" 

perPartesMod :: Integer -> Integer
perPartesMod n
    | mod n 2 == 0 = 0
    | otherwise  = 1

num :: [Integer]
num = [0,1,2]

range :: [Integer]
range = [0..100]

genSeqDesc :: Integer -> [Integer]
genSeqDesc 0 = [0]
genSeqDesc n = n : genSeqDesc (n-1)

genSeqAsc :: Integer -> [Integer]
genSeqAsc n = if n == 0 
                    then [0]
                    else genSeqAsc (n-1) ++ [n]

data Color = 
        Blue
    |   Red
    |   Yellow
    |   Black
    |   White
    deriving Show

color :: Color
color = Black

data FailableDouble =   Fail
                        | OK Double
    deriving Show

d1 :: FailableDouble
d1 = Fail

d2 :: FailableDouble
d2 = OK 2.5

divWFail :: Double -> Double -> FailableDouble
divWFail _ 0 = Fail
divWFail a b = OK (a / b)

data Person = Person String Int
    deriving Show

pepa :: Person
pepa = Person "Pepa" 20

getPersonAge :: Person -> Int
getPersonAge (Person _ age) = age

absAll :: (Num a) => [a] -> [a]
absAll [] = []
absAll (x:xs) = abs(x) : absAll(xs)

len :: [a] -> Integer
len [] = 0
len (_:xs) = 1 + len xs

mergeOrd :: (Ord a ) => [a] -> [a] -> [a]
mergeOrd [] x = x
mergeOrd x [] = x
mergeOrd l1@(x:xs) l2@(y:ys) = if x < y 
                                then x : mergeOrd xs l2
                                else y : mergeOrd l1 ys

fac :: (Num a, Ord a) => a -> a
fac n   | n < 2 = 1
        | otherwise = n * fac (n-1)

revQuad :: [a] -> [a]
revQuad [] = []
revQuad (x:xs) = revQuad xs ++ [x]

revLin :: [a] -> [a]
revLin [] = []
revLin xs = rev xs []
    where   rev [] ys = ys
            rev (x:xs) ys = (rev xs ys) ++ [x]

sumList :: (Num a) => [a] -> a
sumList = foldl (+) 0 

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs


swapPairs :: [a] -> [a]
swapPairs [] = []
swapPairs [x] = [x]
swapPairs (x:y:xs) = y:x:swapPairs xs

{- fib :: Num a => [a]
fib = 0 : 1 : zipWith (+) fib (tail fib) -}

cLists :: [a] -> [a] -> [a]
cLists [] ys = ys
--cLists xs (y:ys) = cLists (xs ++ [y]) ys
cLists (x:xs) ys =  x : cLists xs ys

mEvenList ::  Integral a => [a] -> [a]
mEvenList xs = filter (even) xs

data Vector a = Vec Int [a]
    deriving Show

initVector :: [a] -> Vector a
initVector xs = Vec (length xs) xs

-- dotProd :: Num a => Vector a -> Vector a -> Maybe a
-- dotProd (Vec l1 xs) (Vec l2 ys) = if l1 == l2 
--                                     then Just (sum (length zipWith (*) xs ys))
--                                     else Nothing


data Tree a = List | Tree a (Tree a) (Tree a)
    deriving (Show, Read, Eq)

singleton :: a -> Tree a 
singleton a = Tree a List List

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x List = singleton x
treeInsert x (Tree a left right) 
    | x == a = Tree a left right 
    | x < a  = Tree a (treeInsert x left) right
    | x > a  = Tree a left (treeInsert x right) 


