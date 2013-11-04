module PPrint where

import Control.Lens
import Data.List (intersperse)
import qualified Data.Set as Set
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
    pprint Ten = "10"
    pprint r | points > 9 = [head . show $ r]
             | otherwise = show points
      where points = Set.findMax . rankPoints $ r

instance PPrint Card where
    pprint c = concat [c^.rank.to pprint, c^.suit.to pprint]

instance (PPrint a) => PPrint ([] a) where
    pprint = concat . intersperse " " . map pprint

instance PPrint Dealer where
    pprint = show

instance PPrint Player where
    pprint _ = "You"

instance (PPrint a) => PPrint (CardPlayer a)  where
    pprint p = name ++ "\t(" ++ showing ++ ")\t" ++ cs
      where
        name = p^.cardPlayer.to pprint
        cs = p^.hand.to pprint
        showing = p^.hand.to (show . handPoints)

instance PPrint Game where
    pprint game = unlines
        [ "Cash $" ++ (game^.player.cardPlayer.cash.to show)
        , "Bet $" ++ (game^.bet.to show)
        , game^.dealer.to pprint
        , game^.player.to pprint
        ]

