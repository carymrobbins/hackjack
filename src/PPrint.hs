module PPrint where

import Data.List

import Control.Lens

import Cards
import Game
import Players

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

instance PPrint Game where
    pprint game = unlines
        [ "Cash $" ++ g^.player.cash.to show
        , "Dealer: " ++ g^.dealer.hand.to pprint
        , "        showing " ++ g^.dealer.hand.to (show . getPoints)
        , "Player: " ++ g^.player.hand.to pprint
        , "        showing " ++ g^.player.hand.to (show . getPoints)
        ]
      where
        g = viewGame game

