push x [] = [x]
push x xs = (x:xs)

pop [] = []
pop copy = popping copy [] []
 where
 popping [] opnd optr = (reverse opnd) ++ optr
 popping (x:xs) opnd optr
  | elem x "()+-*/" = popping xs opnd (x:optr)
  | otherwise = popping xs (x:opnd) optr

pop' [] = []
pop' (_:xs) = xs

pop'' [] = []
pop'' (x:xs)
 | not (elem x "()") = x:(pop'' xs)
 | otherwise = pop'' xs 
 
isEmpty [] = True
isEmpty _  = False

delimit [] = True
delimit copy = matching copy []
 where
 matching [] ret = isEmpty ret
 matching (x:xs) ret
  | elem x ")" = matching xs (push x ret)
  | elem x "(" = matching xs (pop' ret)
  | otherwise = matching xs ret
  
main = do
 let expr = "A*(B+C)-D"
 print $ expr
 print $ pop expr
 print $ pop'' (pop expr)
 print $ delimit (pop expr)
