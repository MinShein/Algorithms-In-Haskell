split [] m r = m: (ssort r)
split (x:xs) m r = if x < m
                   then split xs x (m:r)
                   else split xs m (x:r)
ssort [] = []
ssort (x:xs) = split xs x []

main :: IO ()
main = do
 let unsortedList = [10,9..0]
 print.show$ssort unsortedList