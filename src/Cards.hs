module Cards where

import Control.Applicative ((<$>), (<*>))

type Points = Int
type Cards = [Card]

newtype Hand = Hand { getCards :: Cards }
               deriving (Show)

data Card = Card Rank Suit
            deriving (Show, Eq)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
            | Jack | Queen | King | Ace
            deriving (Show, Eq, Enum)

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Show, Eq, Enum)

rank :: Card -> Rank
rank (Card r _) = r

suit :: Card -> Suit
suit (Card _ s) = s

allRanks :: [Rank]
allRanks = [Two ..]

allSuits :: [Suit]
allSuits = [Clubs ..]

allCards :: Cards
allCards = Card <$> allRanks <*> allSuits

class HasPoints a where
    getPoints :: a -> Points

instance HasPoints Card where
    getPoints (Card Ace _) = 11
    getPoints (Card rank _) = min (fromEnum rank + 2) 10

instance HasPoints Hand where
    getPoints Hand { getCards = [] } = 0
    
    getPoints hand =
        if base > 21 && numAces > 0
        then maximum $ filter (<=21) possibleScores
        else base
      where
        base = sum $ map getPoints $ getCards hand
        numAces = length $ filter ((Ace==) . rank) $ getCards hand
        possibleScores = map ((base-) . (*10)) [1..numAces]

