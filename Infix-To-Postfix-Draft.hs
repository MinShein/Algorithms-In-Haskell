push :: Char -> [Char] -> [Char]
push x [] = [x]
push x xs = reverse (x:(reverse xs))

pop :: [Char] -> [Char]
pop [] = []
pop (x:xs) = xs

peek :: [Char] -> Char
peek (x:_) = x

isEmpty :: [Char] -> Bool
isEmpty [] = True
isEmpty _  = False

delimit :: [Char] -> Bool
delimit [] = True
delimit copy = matching copy []
 where
 matching :: [Char] -> [Char] -> Bool
 matching [] ret = isEmpty ret
 matching (x:xs) ret
  | elem x "(" = matching xs (push x ret)
  | elem x ")" = matching xs (pop ret)
  | otherwise = matching xs ret

isParenthesis :: Char -> Bool
isParenthesis chr = elem chr "()"
isOperand :: Char -> Bool
isOperand chr = elem chr ['A'..'Z'] || elem chr ['a'..'z']
isOperator :: Char -> Bool
isOperator chr = elem chr "+-*/^"

infixToPostfix :: [Char] -> [Char]
infixToPostfix expr = helper expr [] []
  where
    helper :: [Char] -> [Char] -> [Char] -> [Char]
    helper [] operand operator = operand ++ operator
    helper copy@(x:xs) operand operator
      | delimit copy == False = error "Delimiter not matched."
      | isParenthesis x = helper xs operand operator
      | isOperand x = helper xs (push x operand) operator
      | isOperator x && isEmpty operator = helper xs operand (x:operator)
      | isOperator x && not (isEmpty operator) =
          if precedence x == precedence (peek operator)
          then helper xs (push (peek operator) operand) (x:pop operator)
          else if precedence x < precedence (peek operator)
          then extractAll xs operand operator x
          else helper xs operand (x:operator)
      | otherwise = helper xs operand operator
    precedence :: Char -> Int
    precedence chr = case chr of
      '+' -> 1
      '-' -> 1
      '*' -> 2
      '/' -> 2
      '^' -> 3
      _   -> 0
    extractAll :: [Char] -> [Char] -> [Char] -> Char -> [Char]
    extractAll xs operand operator x = extractAll' xs operand operator x (length operator)
      where
        extractAll' :: [Char] -> [Char] -> [Char] -> Char -> Int -> [Char]
        extractAll' xs operand [] x 0 = helper xs operand [x]
        extractAll' xs operand (y:ys) x len = extractAll' xs (push y operand) ys x (len-1)

main :: IO ()
main = do
 let expr = "A+B-C*D+E/F*E+G/A"
 print $ infixToPostfix expr
