binarySearchHelper :: (Ord a) => [a] -> a -> Int -> Int -> Maybe Int
binarySearchHelper list target low high
 | low > high = Nothing
 | otherwise = 
    let mid = (low+high) `div` 2
        midValue = list !! mid
    in if midValue == target
       then Just mid
       else if midValue > target
            then binarySearchHelper list target low (mid-1)
            else binarySearchHelper list target (mid+1) high
            
binarySearch :: (Ord a) => [a] -> a -> Maybe Int
binarySearch [] _ = Nothing
binarySearch list target = binarySearchHelper list target 0 ((length list) - 1)

bubbleSwap :: [Integer] -> [Integer]
bubbleSwap [x] = [x]
bubbleSwap (x1:x2:xs)
 | x2 > x1 = x1: bubbleSwap (x2:xs)
 | otherwise = x2: bubbleSwap (x1:xs)
 
bubbleSortHelper :: [Integer] -> Int -> [Integer]
bubbleSortHelper list 0 = list
bubbleSortHelper list n = bubbleSortHelper (bubbleSwap list) (n-1)

bubbleSort :: [Integer] -> [Integer]
bubbleSort [] = []
bubbleSort list = bubbleSortHelper list (length list)

main :: IO ()
main = do
 let sorted = bubbleSort [10,9..0]
 print.show$(sorted)
 print.show.(binarySearch sorted)$8