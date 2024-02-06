quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort [x,y] = ((min x y):(max x y):[])
quicksort (x:xs) =
 quicksort [a| a <- xs, a <= x] ++ [x] ++ quicksort [a| a <- xs, a > x]
 
main = do
 let unsortedList = [1,2222,1111,2,3,4,56,111,1,0]
 print $ quicksort unsortedList