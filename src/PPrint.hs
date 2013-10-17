module PPrint where

import Data.List (intersperse)
import qualified Data.Set
import Cards (Card(..), Rank(..), Suit(..), rankPoints,
              Hand(..), handPoints)

class PPrint a where
    pprint :: a -> String

instance PPrint Suit where
    pprint Spades = "♠" 
    pprint Hearts = "♥"
    pprint Diamonds = "♦"
    pprint Clubs = "♣"

instance PPrint Rank where
    pprint r | (rankPoints r) > 9 = [head . show $ r]
             | otherwise = show . rankPoints $ r

instance PPrint Card where
    pprint (Card r s) = concat [pprint r, pprint s]

instance PPrint Hand where
    pprint = concat . intersperse " " . map pprint . handCards

