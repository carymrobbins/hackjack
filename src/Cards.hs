module Cards where

import Control.Applicative ((<$>), (<*>))

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
    getPoints :: a -> Int

instance HasPoints Card where
    getPoints (Card Ace _) = 11
    getPoints (Card Two _) = 2
    getPoints (Card Three _) = 3
    getPoints (Card Four _) = 4
    getPoints (Card Five _) = 5
    getPoints (Card Six _) = 6
    getPoints (Card Seven _) = 7
    getPoints (Card Eight _) = 8
    getPoints (Card Nine _) = 9
    getPoints _ = 10

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

