
data Rank = Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Enum, Ord)
data Suit = Diamond | Heart | Spade | Club deriving (Show, Eq, Enum, Ord)
data Card = MkCard {
    rank :: Rank,
    suit :: Suit
    } 
    deriving (Show, Eq)

instance Ord Card where
    (<=) :: Card -> Card -> Bool
    (MkCard rank1 suit1) <= (MkCard rank2 suit2) = if rank1 == rank2 then suit1 <= suit2 else rank1 <= rank2

data Hand = MkHand Card Card Card

value :: Hand -> Integer
value (MkHand (MkCard rank1 suit1) (MkCard rank2 suit2) (MkCard rank3 suit3)) 
    | suit1 == suit2 && suit1 == suit3 && suit2 == suit3 = 3
    | rank1 == rank2 && rank1 == rank3 && rank2 == rank3 = 2
    | rank1 == rank2 || rank1 == rank3 || rank2 == rank3 = 1
    | otherwise = 0

myHand :: Hand
myHand = MkHand card1 card2 card3

card1 :: Card
card1 = MkCard King Heart

card2 :: Card
card2 = MkCard Nine Heart

card3 :: Card
card3 = MkCard Seven Heart