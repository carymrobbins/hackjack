module Cards where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate)

type Points = Int

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
            | Jack | Queen | King | Ace
            deriving (Show, Eq, Enum)

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Show, Eq, Enum)

data Card = Card { rank :: Rank
                 , suit :: Suit
                 }
            deriving (Show, Eq)

type Cards = [Card]

data Hand = Hand { handCards :: Cards }
            deriving (Show)

allRanks :: [Rank]
allRanks = [Two ..]

allSuits :: [Suit]
allSuits = [Clubs ..]

allCards :: Cards
allCards = Card <$> allRanks <*> allSuits

cardPoints (Card Ace _) = 11
cardPoints (Card r _) = min (fromEnum r + 2) 10

handPoints (Hand []) = 0

handPoints hand =
    if base > 21 && numAces > 0
    then maximum $ filter (<=21) possibleScores
    else base
  where
    base = sum $ map cardPoints $ handCards hand
    numAces = length $ filter ((Ace==) . rank) $ handCards hand
    possibleScores = map ((base-) . (*10)) [1..numAces]

