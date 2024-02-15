lsearch [] target = -1
lsearch list target = lsearch' list target 0 (length list)

lsearch' [] target acc 0 = -1
lsearch' (x:xs) target acc n
 | target == x = acc
 | otherwise = lsearch' xs target (acc+1) (n-1)
 
bsearch [] _ = -1
bsearch list target = bsearch' list target 0 (length list)

bsearch' [] target low high = -1
bsearch' list target low high
 | low > high = -1
 | target == middleValue = mid
 | target < middleValue = bsearch' list target low (mid-1)
 | otherwise = bsearch' list target (mid+1) high
 where
  mid = (low + high) `div` 2
  middleValue = list !! mid

bsort [] = []
bsort list = bsort' list (length list)

bsort' list 0 = list
bsort' list n = bsort' (swap' list) (n-1)

swap' [x] = [x]
swap' (x1:x2:xs)
 | x1 < x2 = x1:(swap' (x2:xs))
 | otherwise = x2:(swap' (x1:xs))

msort [] = []
msort [x] = [x]
msort [x,y] = [min x y,max x y]
msort list = merge (msort listL) (msort listR)
 where
  listL = take splitPoint list
  listR = drop splitPoint list
  splitPoint = (length list) `div` 2

merge left [] = left
merge [] right = right
merge left@(x:xs) right@(y:ys)
 | x <= y = x:merge xs right
 | otherwise = y:merge left ys
 
qsort [] = []
qsort [x] = [x]
qsort [x,y] = [min x y,max x y]
qsort (x:xs) = qsort [a| a <- xs, a <= x] ++ [x] ++ qsort [a| a <- xs, a > x]

swap [] m r = m:(ssort r)
swap (x:xs) m r
 | m < x = swap xs m (x:r)
 | otherwise = swap xs x (m:r)

ssort [] = []
ssort (y:ys) = swap ys y []

insert key [] = [key]
insert key l@(x:xs)
 | key < x = key:l
 | otherwise = x:(insert key xs)

isort [] = []
isort list = foldr insert [] list

main :: IO ()
main = do
 let unsortedList = [9,2,2,2,1,1,0,-1,100,8,2,9,1]
 print $ "Insertion Sort"
 print $ isort unsortedList
 print $ ssort unsortedList
 print $ qsort unsortedList
 print $ msort unsortedList
 print $ bsort unsortedList
 print $ bsearch (qsort unsortedList) 100
 print $ lsearch (msort unsortedList) 100
