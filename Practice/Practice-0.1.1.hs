selectionSort [] = []
selectionSort (x:xs) = split xs x []

split [] m r = m:(selectionSort r)
split (x:xs) m r
 | x < m = split xs x (m:r)
 | otherwise = split xs m (x:r)

bubbleSwap [] = []
bubbleSwap [x] = [x]
bubbleSwap (x1:x2:xs)
 | x2 > x1 = x1:bubbleSwap (x2:xs)
 | otherwise = x2:bubbleSwap (x1:xs)
 
bubbleSortHelper list 0 = list
bubbleSortHelper list n = bubbleSortHelper (bubbleSwap list) (n-1)

bubbleSort [] = []
bubbleSort list = bubbleSortHelper list (length list)

foldr' f s [] = s
foldr' f s (x:xs) = f x (foldr' f s xs)

insert key [] = [key]
insert key l@(x:xs)
 | key <= x = key:l
 | otherwise = x:(insert key xs)

insertionSort [] = []
insertionSort list = foldr' insert [] list

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort [x,y] = (min x y):(max x y):[]
quickSort (x:xs) = quickSort ([a| a <- xs, a <= x]) ++ [x] ++ quickSort ([a| a <- xs, a > x])

linearSearch [] target = -1
linearSearch list target = linearSearchHelper list target 0 (length list)

linearSearchHelper [] target index 0 = -1
linearSearchHelper (x:xs) target index n 
 | target == x = index
 | otherwise = linearSearchHelper xs target (index+1) (n-1)

binarySearch [] target = -1
binarySearch list target = binarySearchHelper list target 0 (length list - 1)

binarySearchHelper [] target low high = -1
binarySearchHelper list target low high
 | low > high = -1
 | target == middleValue = middle
 | target > middleValue = binarySearchHelper list target (middle+1) high
 | otherwise = binarySearchHelper list target low (middle-1)
 where
  middle = (low+high) `div` 2
  middleValue = list !! middle
  
main = do
 let unsortedList = [8,2,3,11,23,4,56,11,233,43]
     sortedList = quickSort unsortedList
 print $ ("ssort :",selectionSort unsortedList)
 print $ ("bsort :",bubbleSort unsortedList)
 print $ ("isort :",insertionSort unsortedList)
 print $ ("qsort :",quickSort unsortedList)
 print $ ("lsearch :",linearSearch sortedList 233)
 print $ ("bsearch :",binarySearch sortedList 233)