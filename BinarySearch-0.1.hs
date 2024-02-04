binarySearch :: [Int] -> Int -> Int
binarySearch [] _ = -1
binarySearch list target = binarySearchHelper list target 0 (length list - 1)

binarySearchHelper :: [Int] -> Int -> Int -> Int -> Int
binarySearchHelper list target lower higher
 | lower > higher = -1
 | target == middle_value = middle
 | target > middle_value = binarySearchHelper list target (middle+1) higher
 | otherwise = binarySearchHelper list target lower (middle-1)
 where 
  middle = (lower+higher) `div` 2
  middle_value = list !! middle

main :: IO ()
main = do
 let sortedList = [1,2..10]
 print$binarySearch sortedList 5
 print$binarySearch sortedList (-1)
 print$binarySearch sortedList 0
 print$binarySearch sortedList 11