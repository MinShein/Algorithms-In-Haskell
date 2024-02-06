mergeSort [] = []
mergeSort [x] = [x]
mergeSort [x,y] = (min x y):(max x y):[]
mergeSort list = merge (mergeSort leftL) (mergeSort rightL)
 where
  leftL = take splitPoint list
  rightL = drop splitPoint list
  splitPoint = (length list) `div` 2
  
merge left [] = left
merge [] right = right
merge left@(x:xs) right@(y:ys) = if x < y
                                 then x:(merge xs right)
                                 else y:(merge left ys)
                                 
main = do
 let unsortedList = [2,33,2,1,1,1,2222,33,444,20,10,20,30,10]
 print $ mergeSort unsortedList