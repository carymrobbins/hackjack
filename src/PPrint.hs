module PPrint where

import Control.Lens
import Data.List (intersperse)
import qualified Data.Set as Set
import Cards (Card(..), Rank(..), Suit(..), rankPoints, handPoints)
import Game (Game(..))
import Players 

class PPrint a where
    pprint :: a -> String

instance PPrint Suit where
    pprint Spades = "♠" 
    pprint Hearts = "♥"
    pprint Diamonds = "♦"
    pprint Clubs = "♣"

instance PPrint Rank where
    pprint r | (Set.findMax . rankPoints $ r) > 9 = [head . show $ r]
             | otherwise = show . Set.findMax . rankPoints $ r

instance PPrint Card where
    pprint (Card r s) = concat [pprint r, pprint s]

instance (PPrint a) => PPrint ([] a) where
    pprint = concat . intersperse " " . map pprint

instance PPrint Dealer where
    pprint = show

instance PPrint Player where
    pprint _ = "You"

instance (PPrint a) => PPrint (CardPlayer a)  where
    pprint p = name ++ "\t(" ++ showing ++ ")\t" ++ cs
      where
        name = pprint $ p^.cardPlayer
        cs = pprint $ p^.hand
        showing = show . handPoints $ p^.hand

instance PPrint Game where
    pprint game = unlines
        [ "Cash $" ++ (show . cash . cardPlayer . player $ game)
        , "Bet $" ++ (show . bet $ game)
        , pprint . dealer $ game
        , pprint . player $ game
        ]

