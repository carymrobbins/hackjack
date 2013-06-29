{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Players where

import Cards (Card, Hand(..), HasPoints, getPoints)

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

instance (CardPlayer a) => HasPoints a where
    getPoints = getPoints . getHand 

class CardPlayer a where
    getHand :: a -> Hand
    
    viewHand :: TurnIsComplete -> a -> Hand
    
    grabCard :: a -> Card -> a
    
    hasBlackjack :: a -> Bool
    hasBlackjack player = getPoints player == 21 &&
                          numCards player == 2
      where
        numCards = length . getCards . getHand 

    busts :: a -> Bool
    busts = (>21) . getPoints

instance CardPlayer Dealer where
    getHand (Dealer hand) = hand
    
    viewHand False = Hand . tail . getCards . getHand
    viewHand True = getHand
    
    grabCard (Dealer (Hand hand)) card = Dealer $ Hand (card:hand)

instance CardPlayer Player where
    getHand (Player hand _) = hand
    
    viewHand _ = getHand
    
    grabCard (Player (Hand hand) cash) card = Player (Hand (card:hand)) cash
