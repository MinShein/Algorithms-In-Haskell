{-# LANGUAGE ExistentialQuantification #-}

class MyClass a where
 myShow :: a -> String

data Showable = forall a.(MyClass a) => Showable a

instance MyClass Showable where
 myShow (Showable a) = myShow a
 
data CustomType = CustomInt Int | CustomString String

instance MyClass CustomType where
 myShow (CustomInt int) = show int
 myShow (CustomString string) = string
 
main = do
 let myShowInt = Showable (CustomInt 30)
     myShowString = Showable (CustomString "Hello!")
 putStrLn.myShow$myShowInt
 putStrLn.myShow$myShowString