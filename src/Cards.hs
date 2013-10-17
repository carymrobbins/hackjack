module Cards where

import Control.Applicative ((<$>), (<*>))
import Data.List (intercalate, sort)
import Data.Set (Set)
import qualified Data.Set as Set

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

cardPoints :: Card -> Set Int
cardPoints (Card Ace _) = Set.fromList [1, 11]
cardPoints (Card r _) = Set.fromList [min (fromEnum r + 2) 10]

handPoints :: Hand -> Int
handPoints (Hand []) = 0
handPoints (Hand cs) = bestPoints . possiblePoints $ cs

possiblePoints :: Cards -> Set Int
possiblePoints [] = Set.singleton 0
possiblePoints cs =
    Set.fromList . map sum . sequence $ map (Set.toList . cardPoints) cs

bestPoints :: Set Int -> Int
bestPoints set
    | Set.null set = 0
    | otherwise = head $ (Set.toDescList good) ++ (Set.toAscList bust)
  where
    (good, bust) = Set.partition (<= 21) set

