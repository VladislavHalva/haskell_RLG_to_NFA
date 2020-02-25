import System.Environment
import Prelude


main :: IO()
main = do
    putStrLn "What";
    a <- getArgs;
    print a;


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



data Vector a = Vec Int [a]
    deriving Show

initVector :: [a] -> Vector a
initVector a = Vec (length a) a 

data Tree = Leaf Int | Node Tree Int Tree
    deriving Show

tree :: Tree
tree = Node (Leaf 2) 1 (Node (Leaf 3) 4 (Leaf 5))

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

