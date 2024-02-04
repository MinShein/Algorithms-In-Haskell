-- Created by Min Thuta Shein
-- Binary Search Algorithm
-- 2/4/2024

binarySearch :: [Int] -> Int -> Int
binarySearch [] _ = -1
binarySearch list target = binarySearchHelper list target 0 (length list - 1)

binarySearchHelper :: [Int] -> Int -> Int -> Int -> Int
binarySearchHelper list target lower higher
  | lower > higher = -1
  | target == middleValue = middle
  | target < middleValue = binarySearchHelper list target lower (middle - 1)
  | otherwise = binarySearchHelper list target (middle + 1) higher
  where
    middle = (lower + higher) `div` 2
    middleValue = list !! middle

main :: IO ()
main = do
  let sortedList = [1,2..10]
  print $ binarySearch sortedList (10)
