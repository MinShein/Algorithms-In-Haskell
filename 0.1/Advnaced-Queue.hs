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

    let deq1  = dequeue enq3
        deq2  = dequeue enq2
        deq3  = dequeue enq1

    print $ isEmpty deq3
