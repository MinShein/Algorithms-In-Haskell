p '+' = 1
p '-' = 1
p '*' = 2
p '/' = 2
p '^' = 3
p _   = 0

tokenizer [] postfix stack = reverse (postfix) ++ stack
tokenizer (x:xs) postfix stack
 | not (null stack) && (head stack) == '^'
 = tokenizer xs ((reverse(take (length stack) stack))++postfix) (drop (length stack) stack)
 | x `elem` "("
 = tokenizer xs postfix (x:stack)
 | x `elem` ")"
 = tokenizer xs ((takeWhile (/= '(') stack)++postfix) (tail (dropWhile (/= '(') stack))
 | x `elem` "+-*/^" && null stack
 = tokenizer xs postfix (x:stack)
 | x `elem` ['a'..'z']
 = tokenizer xs (x:postfix) stack
 | x `elem` "+-*/^" && not (null stack)
 = if p x > p (head stack)
   then tokenizer xs postfix (x:stack)
   else if p x <= p (head stack)
   then tokenizer xs ((head stack):postfix) (x:tail stack)
   else tokenizer xs postfix stack
 | otherwise 
 = tokenizer xs postfix stack

main = do
 print "Infix to Postfix"
 print $ tokenizer "a*b+c/d^e-f*g" [] []
