bubbleSwap :: (Ord a) => [a] -> [a]
bubbleSwap [x] = [x]
bubbleSwap (x1:x2:xs)
 | x2 > x1 = x1 : bubbleSwap (x2:xs)
 | otherwise = x2: bubbleSwap (x1:xs)
 
bubbleSortHelper :: (Ord a) => [a] -> Int -> [a]
bubbleSortHelper list 0 = list
bubbleSortHelper list n = bubbleSortHelper (bubbleSwap list) (n-1)

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort list = bubbleSortHelper list (length list)