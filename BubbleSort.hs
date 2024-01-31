bubbleSort :: [Integer] -> [Integer]
bubbleSort [] = []
bubbleSort list = bubbleSort' list (length list)

bubbleSort' :: [Integer] -> Int -> [Integer]
bubbleSort' list 0 = list
bubbleSort' list n = bubbleSort' (bubbleSwap list) (n-1)

bubbleSwap :: [Integer] -> [Integer]
bubbleSwap [x] = [x]
bubbleSwap (x1:x2:xs) 
 | x2 > x1 = x1 : bubbleSwap (x2:xs)
 | otherwise = x2: bubbleSwap (x1:xs)

main :: IO ()
main = do
 print.show$bubbleSort [3,2,1]