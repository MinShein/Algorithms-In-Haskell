type Queue = [Int]

-- Function to create an empty queue
createQueue :: Queue
createQueue = []

-- Function to enqueue an item into the queue
enqueue :: Int -> Queue -> Queue
enqueue item queue = [item] ++ queue

-- Function to dequeue an item from the queue
dequeue :: Queue -> Queue
dequeue [] = []     -- Handle case where the queue is empty
dequeue (_:xs) = xs

-- Function to check if the queue is empty
isEmpty :: Queue -> Bool
isEmpty = null

-- Main function
main :: IO ()
main = do
    -- Create an empty queue
    let queue = createQueue
    -- Enqueue an item (1 in this case)
    let enqueued = enqueue 1 queue
    -- Dequeue an item
    let dequeued = dequeue enqueued
    -- Check if the queue is empty after dequeuing
    let isEmptyResult = isEmpty dequeued

    -- Print the results
    putStrLn "Queue:"
    print enqueued
    print dequeued
    print isEmptyResult
