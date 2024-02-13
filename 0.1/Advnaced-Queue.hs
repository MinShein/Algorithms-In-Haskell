{-# LANGUAGE ExistentialQuantification #-}
data List a = Empty | Cons !a !(List a) deriving (Show,Eq)

createQueue :: List a -> List a
createQueue Empty = Empty
createQueue (Cons a b) = Cons a b

enqueue :: a -> List a -> List a
enqueue x Empty = Cons x Empty
enqueue x (Cons a b) = Cons x (Cons a b)

dequeue :: List a -> List a
dequeue Empty = Empty
dequeue list = dequeue' (reverseList list Empty)

dequeue' :: List a -> List a
dequeue' (Cons a b) = reverseList b Empty

isEmpty :: forall a.(Eq a) => List a -> Bool
isEmpty Empty = True
isEmpty (Cons a Empty) = False
isEmpty (Cons a b)
    | b /= Empty = False
    | otherwise = isEmpty b

reverseList :: List a -> List a -> List a
reverseList Empty r = r
reverseList (Cons a b) r = reverseList b (Cons a r)

main :: IO ()
main = do
    let queue = createQueue Empty

    let enq1  = enqueue 1 queue
        enq2  = enqueue 2 enq1
        enq3  = enqueue 3 enq2

    putStrLn "Initial Queue:"
    printQueue enq3

    let deq1  = dequeue enq3
    putStrLn "\nAfter Dequeue 1:"
    printQueue deq1

    let deq2  = dequeue deq1
    putStrLn "\nAfter Dequeue 2:"
    printQueue deq2

    let enq4  = enqueue 4 deq2
    putStrLn "\nAfter Enqueue 4:"
    printQueue enq4

    let deq3  = dequeue enq4
    putStrLn "\nAfter Dequeue 3:"
    printQueue deq3
    
    putStrLn "\nAfter Dequeue 4:"
    putStrLn "Is the Queue Empty?"
    print $ isEmpty (dequeue deq3)

-- Helper function to print the elements of the queue
printQueue :: Show a => List a -> IO ()
printQueue Empty = return ()
printQueue (Cons a Empty) = print $ a
printQueue (Cons a b) = do
    putStr $ show a ++ " "
    printQueue b
