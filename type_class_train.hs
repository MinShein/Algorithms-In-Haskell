data BinaryOperations a b = Plus a | Multiply b | BinaryOperations a b deriving (Show,Eq,Ord)
class Mathematics b where
 plus :: b -> b -> b
 mult :: b -> b -> b
 complex :: b -> b -> b
 bmap :: (a->b) -> Maybe a -> Maybe b
 
instance (Num a,Num b) => Mathematics (BinaryOperations a b) where
 plus (Plus a) (Plus b) = Plus (a+b)
 mult (Multiply a) (Multiply b) = Multiply (a*b)
 complex (BinaryOperations a b) (BinaryOperations c d) = BinaryOperations (a+c) (b+d)
 bmap (g) (Just y) = Just (g y)
 
main :: IO ()
main = do
  let p = plus (Plus 1) (Plus 2)
  let m = mult (Multiply 2) (Multiply 3)
  let c = complex (BinaryOperations 3 6) (BinaryOperations 5 7)
  let b = bmap (\x-> BinaryOperations (x+x) (x-x)) (Just 12)
  print.show$p
  print.show$m
  print.show$c
  print.show$b