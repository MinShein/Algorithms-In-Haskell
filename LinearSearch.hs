linearSearch :: (Eq a) => [a] -> a -> Maybe Int
linearSearch [] _ = Nothing
linearSearch (x:xs) target
 | target == x = Just 0
 | otherwise = case linearSearch xs target of
               Just index -> Just (index+1)
               Nothing -> Nothing