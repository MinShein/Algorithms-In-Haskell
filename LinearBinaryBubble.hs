linearSearch :: (Eq a) => [a] -> a -> Maybe Int
linearSearch [] _ = Nothing
linearSearch (x:xs) target
 | target == x = Just 0
 | otherwise = case linearSearch xs target of
               Just index -> Just (index+1)
               Nothing -> Nothing

binarySearch :: (Ord a) => [a] -> a -> Maybe Int
binarySearch [] _ = Nothing
binarySearch list target = binarySearchHelper list target 0 (length list - 1)
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

main :: IO ()
main = do
 let list = [1,2,3,4,5,6,7,8,9,10]
     sorted = bubbleSort list
 print.show$linearSearch list 10
 print.show$binarySearch list 10