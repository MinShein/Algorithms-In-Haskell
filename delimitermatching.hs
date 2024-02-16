push item [] = [item]
push item stack = item:stack

pop [] = []
pop (x:xs) = xs

isEmpty [] = True
isEmpty (_:_) = False

dem [] stack = isEmpty(stack)
dem (x:xs) stack
 | x == '[' = dem xs (push x stack)
 | x == ']' = (not (isEmpty stack)) && dem xs (pop stack)
 | otherwise = dem xs stack
 
main = do
 print $ dem "[[[a]lko][gjk[lll[<>]]oop]]][[" []
 print $ dem "[ab[cd]ef]gh" []
