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

main :: IO ()
main = do
 let elementToSearch = [1,2,3,4,5,6,7,8,9,10]
 input <- getLine
 let target = read input :: Int
 print.show$binarySearch elementToSearch target
