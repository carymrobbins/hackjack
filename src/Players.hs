module Players where

import Control.Lens
import Cards (Hand(..), Points, handPoints)

type Cash = Int

data Dealer = Dealer Hand deriving (Show)
data Player = Player Hand Cash deriving (Show)

dealerHitMax :: Points
dealerHitMax = 17

newDealer :: Dealer
newDealer = Dealer []

newPlayer :: Player
newPlayer = Player [] 100

cash :: Lens' Player Cash
cash = lens getCash setCash
  where
    getCash (Player _ c) = c
    setCash (Player h _) c = Player h c

class CardPlayer a where
    getHand :: a -> Hand
    setHand :: a -> Hand -> a

    hand :: Lens' a Hand
    hand = lens getHand setHand

    playerPoints :: a -> Int
    playerPoints p = p^.hand.to handPoints

    hasBlackjack :: a -> Bool
    hasBlackjack p = playerPoints p == 21 && p^.hand.to length == 2

    busts :: a -> Bool
    busts p = playerPoints p > 21

instance CardPlayer Dealer where
    getHand (Dealer h) = h
    setHand _ h = Dealer h

instance CardPlayer Player where
    getHand (Player h _) = h
    setHand (Player _ c) h = Player h c

