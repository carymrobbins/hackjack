module Players where

import Cards (Card, Hand(..), getPoints)

type Cash = Int
type TurnIsComplete = Bool

data Dealer = Dealer Hand deriving (Show)
data Player = Player Hand Cash deriving (Show)

newDealer :: Dealer
newDealer = Dealer $ Hand []

newPlayer :: Player
newPlayer = Player (Hand []) 0

getCash :: Player -> Cash
getCash (Player _ cash) = cash

class CardPlayer a where
    getHand :: a -> Hand
    
    viewHand :: TurnIsComplete -> a -> Hand
    
    grabCard :: a -> Card -> a
    
    hasBlackjack :: a -> Bool
    hasBlackjack player = (getPoints . getHand) player == 21 &&
                          (length . getCards . getHand) player == 2
    busts :: a -> Bool
    busts = (>21) . getPoints . getHand

instance CardPlayer Dealer where
    getHand (Dealer hand) = hand
    
    viewHand False = Hand . tail . getCards . getHand
    viewHand True = getHand
    
    grabCard (Dealer (Hand hand)) card = Dealer $ Hand (card:hand)

instance CardPlayer Player where
    getHand (Player hand _) = hand
    
    viewHand _ = getHand
    
    grabCard (Player (Hand hand) cash) card = Player (Hand (card:hand)) cash
