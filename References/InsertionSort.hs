foldr' :: (Int -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int]
foldr' _ empty [] = empty
foldr' insert empty (x:xs) = (insert x) (foldr' insert empty xs)

insert :: Int -> [Int] -> [Int]
insert key [] = [key]
insert key l@(x:xs)
 | key <= x = key:l
 | otherwise = x: (insert key xs)
 
isort :: [Int] -> [Int]
isort list = foldr' insert [] list

main :: IO ()
main = do
 let unsortedList = [10,9..0]
 print.show$isort unsortedList