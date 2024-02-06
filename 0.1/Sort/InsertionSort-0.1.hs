-- Created by Min Thuta Shein
-- Insertion Sort Algorithm
-- 3-4-2024
-- Insertion Sort Like Poker Game

folding_the_cards :: (Ord card) => (card -> [card] -> [card]) -> [card] -> [card] -> [card]
folding_the_cards _ hand [] = hand
folding_the_cards insert hand (currentCard:remainedCard) = (insert currentCard) (folding_the_cards insert hand remainedCard)

insert :: (Ord card) => card -> [card] -> [card]
insert nextCard [] = [nextCard]
insert nextCard sorted@(currentCard:remainedCards)
 | nextCard <= currentCard = nextCard:sorted
 | otherwise = currentCard: (insert nextCard remainedCards)

sort_the_cards_by_insertion unsortedCards = folding_the_cards insert [] unsortedCards

main :: IO ()
main = do
 let unsortedCards = [13,15,1,9,7,8,8,11,3,2,2,4,3,4,5,10,5]
 print.show$sort_the_cards_by_insertion unsortedCards