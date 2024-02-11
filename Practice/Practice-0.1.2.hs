power :: Int -> Int -> Int
power x 0 = 1
power x p 
    | even p = n * n
    | otherwise = n * n * x
    where
        n = power x (div p 2)
        
isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = not (hasDivisor (x-1))
    where
        hasDivisor :: Int -> Bool
        hasDivisor 1 = False
        hasDivisor n = mod x n == 0 || hasDivisor(n-1)
        
swap :: [Int] -> Int -> [Int] -> [Int]
swap [] current stack = current:(ssort stack)
swap (next:remained) current stack
    | current > next = swap remained next (current:stack)
    | otherwise = swap remained current (next:stack)
ssort :: [Int] -> [Int]     
ssort [] = []
ssort (current:remained) = swap remained current []

foldr' :: (Int -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int]
foldr' insert key [] = []
foldr' insert key (current:remained) = insert current (foldr' insert key remained)
insert :: Int -> [Int] -> [Int]
insert key [] = [key]
insert key ph@(current:remained)
    | key < current = key:ph
    | otherwise = current:(insert key remained)
isort :: [Int] -> [Int]
isort [] = []
isort list = foldr' insert [] list

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort [x,y] = (min x y):(max x y):[]
mergeSort list = merge (mergeSort leftL) (mergeSort rightL)
    where
        leftL = take splitPoint list
        rightL = drop splitPoint list
        splitPoint = (length list) `div` 2
merge :: [Int] -> [Int] -> [Int]
merge left [] = left
merge [] right = right
merge left@(x:xs) right@(y:ys) = if x < y
                                 then x:(merge xs right)
                                 else y:(merge left ys)

bubbleSort :: [Int] -> [Int]
bubbleSort [] = []
bubbleSort list = bubbleSort' list (length list)
bubbleSort' :: [Int] -> Int -> [Int]
bubbleSort' list 0 = list
bubbleSort' list n = bubbleSort' (swap' list) (n-1)
swap' :: [Int] -> [Int]
swap' [] = []
swap' [x] = [x]
swap' (x1:x2:xs)
    | x2 > x1 = x1:(swap' (x2:xs))
    | otherwise = x2:(swap' (x1:xs))
 
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort [x] = [x]
quickSort [x,y] = [(min x y),(max x y)]
quickSort (x:xs) = quickSort lower ++ pivot ++ quickSort higher
    where
        pivot = [x]
        lower = [low| low <- xs, low <= x]
        higher = [high| high <- xs,high > x]

main :: IO ()
main = do
 let unsortedList = [2,1,3,0,4,18,9,9,9,-10]
 print $ power 2 3
 print $ isPrime 7
 print $ ssort unsortedList
 print $ isort unsortedList
 print $ mergeSort unsortedList
 print $ bubbleSort unsortedList
 print $ quickSort unsortedList