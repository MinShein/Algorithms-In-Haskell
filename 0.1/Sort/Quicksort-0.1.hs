quickSort [] = []
quickSort [x] = [x]
quickSort [x,y] = (min x y):(max x y):[]
quickSort (x:xs) = quickSort lower ++ pivot ++ quickSort higher
 where
     pivot = [x]
     lower = [low| low <- xs, low <= x]
     higher = [high| high <- xs, high > x]
