module PPrint where

import Data.List

import Cards

class PPrint a where
    pprint :: a -> String

instance PPrint Suit where
    pprint Spades = "♠" 
    pprint Hearts = "♥"
    pprint Diamonds = "♦"
    pprint Clubs = "♣"

instance PPrint Rank where
    pprint r | getPoints r > 9 = [head . show $ r]
             | otherwise = show . getPoints $ r

instance PPrint Card where
    pprint (Card r s) = concat [pprint r, pprint s]

instance PPrint Hand where
    pprint = concat . intersperse " " . map pprint . _cards

