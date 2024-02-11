quickSort [] = []
quickSort [x] = [x]
quickSort [x,y] = (min x y):(max x y):[]
quickSort (x:xs) = quickSort lower ++ pivot ++ quickSort higher
 where
     pivot = [x]
     lower = [a| a <- xs, a <= x]
     higher = [a| a <- xs, a > x]
