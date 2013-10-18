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

type Hand = [Card]

allRanks :: [Rank]
allRanks = [Two ..]

allSuits :: [Suit]
allSuits = [Clubs ..]

allCards :: [Card]
allCards = Card <$> allRanks <*> allSuits

rankPoints :: Rank -> Set Int
rankPoints Ace = Set.fromList [1, 11]
rankPoints r = Set.fromList [min (fromEnum r + 2) 10]

handPoints :: Hand -> Points
handPoints [] = 0
handPoints cs = bestPoints . possiblePoints $ cs

possiblePoints :: [Card] -> Set Points
possiblePoints [] = Set.singleton 0
possiblePoints cs =
    Set.fromList . map sum . sequence $
        map (Set.toList . rankPoints . rank) cs

bestPoints :: Set Points -> Points
bestPoints set
    | Set.null set = 0
    | otherwise = head $ (Set.toDescList good) ++ (Set.toAscList bust)
  where
    (good, bust) = Set.partition (<= 21) set

