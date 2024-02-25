p '+' = 1
p '-' = 1
p '*' = 2
p '/' = 2
p '^' = 3
p _   = 0

tokenizer [] postfix stack = reverse postfix ++ stack
tokenizer (x:xs) postfix stack
 | x `elem` "abcdefghijklmnopqrstuvwxyz" = tokenizer xs (x:postfix) stack
 | x `elem` "+-*/^"
 = let (s1, s2) = span (\op -> p x <= p op && op /= '(') stack
   in tokenizer xs (reverse s1 ++ postfix) (x:s2)
 | x == '(' = tokenizer xs postfix (x:stack)
 | x == ')'
 = let (s1, _:s2) = break (== '(') stack
   in tokenizer xs (reverse s1 ++ postfix) s2
 | otherwise = tokenizer xs postfix stack

main = print $ tokenizer "(a^b)-(c*d)/(e*f)+(g^(-e))" [] []
