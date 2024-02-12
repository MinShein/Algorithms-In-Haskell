data List a = Empty | Cons !a !(List a) deriving (Show,Eq)

createQueue :: List a -> List a
createQueue Empty = Empty
createQueue (Cons a b) = Cons a b

enqueue :: a -> List a -> List a
enqueue x Empty = Cons x Empty
enqueue x (Cons a b) = Cons x (Cons a b)

dequeue :: List a -> List a
dequeue Empty = Empty
dequeue (Cons a b) = b

isEmpty :: forall a.(Eq a) => List a -> Bool
isEmpty Empty = True
isEmpty (Cons a b)
    | b /= Empty = False
    | otherwise = isEmpty b

main :: IO ()
main = do
    let queue = createQueue Empty
        enq1  = enqueue 1 queue
        enq2  = enqueue 2 enq1
        enq3  = enqueue 3 enq2
        deq1  = dequeue enq3
        deq2  = dequeue enq2
        deq3  = dequeue enq1

    print $ isEmpty deq3
