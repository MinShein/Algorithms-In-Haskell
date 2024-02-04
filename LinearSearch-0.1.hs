-- Created by Min Thuta Shein
-- Linear Search Algorithm
-- 2/4/2024

linearSearch :: [Int] -> Int -> Int
linearSearch [] _ = -1
linearSearch list target = linearSearchHelper list target 0 (length list)

linearSearchHelper :: [Int] -> Int -> Int -> Int -> Int
linearSearchHelper [] target acc size = (linearSearch [] 0)
linearSearchHelper (x:xs) target acc size
 | target == x = acc
 | otherwise = linearSearchHelper xs target (acc+1) (size-1)
               
main :: IO ()
main = do
 let sortedList = [1,2..10]
 print.show$linearSearch sortedList 0