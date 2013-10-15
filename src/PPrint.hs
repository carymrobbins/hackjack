module PPrint where

import Cards (getPoints, Card(..), Rank(..), Suit(..))

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

