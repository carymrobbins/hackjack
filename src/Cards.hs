{-# LANGUAGE TemplateHaskell #-}
module Cards where

import Control.Applicative ((<$>), (<*>))
import Control.Lens (makeLenses)
import Data.List (intercalate)
import Text.Printf (printf)

type Points = Int

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
            | Jack | Queen | King | Ace
            deriving (Show, Eq, Enum)

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Show, Eq, Enum)

data Card = Card Rank Suit
            deriving (Show, Eq)

type Cards = [Card]

data Hand = Hand { _cards :: Cards }
            deriving (Show)

makeLenses ''Hand

showHand :: Hand -> String
showHand hand =
        intercalate " " cs
      where
        cs = map showCard $ _cards hand

showCard :: Card -> String
showCard card =
    printf "[%s %s]" strRank strSuit
  where
    strRank = show . rank $ card
    strSuit = show . suit $ card

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
    getPoints (Card r _) = min (fromEnum r + 2) 10

instance HasPoints Hand where
    getPoints (Hand []) = 0
    
    getPoints hand =
        if base > 21 && numAces > 0
        then maximum $ filter (<=21) possibleScores
        else base
      where
        base = sum $ map getPoints $ _cards hand
        numAces = length $ filter ((Ace==) . rank) $ _cards hand
        possibleScores = map ((base-) . (*10)) [1..numAces]

