-- Created by Min Thuta Shein
-- Binary Search Algorithm
-- 2/4/2024

binarySearch :: [Int] -> Int -> Int
binarySearch [] _ = -1
binarySearch list target = binarySearchHelper list target 0 (length list - 1) -- higher is (length list - 1) because index of the list is always less than the cardinality of the list eg. index of the list is 9 and cardinality of the list is 10

binarySearchHelper :: [Int] -> Int -> Int -> Int -> Int
binarySearchHelper list target lower higher
  | lower > higher = -1 -- if the lower index value is greater than higher it is a negative case
  | target == middleValue = middle -- if the target value is equal to middleValue it is a positive case
  | target < middleValue = binarySearchHelper list target lower (middle - 1) -- if the target value is less than the middleVlue find the target in the left part of the list
  | otherwise = binarySearchHelper list target (middle + 1) higher -- if the target value is greater than the middleValue find the target value in the right part of the list
  where
    middle = (lower + higher) `div` 2 -- compute middle index 
    middleValue = list !! middle      -- compute middle value

main :: IO ()
main = do
  let sortedList = [1,2..10]
  print $ binarySearch sortedList (-1) -- negative case
  print $ binarySearch sortedList 0    -- negative case
  print $ binarySearch sortedList 5    -- positive case
  print $ binarySearch sortedList 11   -- negative case