-- Created by Min Thuta Shein
-- Selection Sort Algorithm
-- 2-4-2024

selectionSort :: [Int] -> [Int]
selectionSort [] = []
selectionSort (x:xs) = split xs x []

split :: [Int] -> Int -> [Int] -> [Int]
split [] current_minimum stack = current_minimum: (selectionSort stack)
split (x:xs) current_minimum stack
 | x < current_minimum = split xs x (current_minimum:stack)
 | otherwise = split xs current_minimum (x:stack)
 
main :: IO ()
main = do
 let unsortedList = [10,9,2222,3,0,1102,22,10,0,0,0]
     sortedList = selectionSort unsortedList
 print.show$ sortedList