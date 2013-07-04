{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Players where

import Cards (Card, Hand(..), HasPoints, getPoints, showCard)

type Cash = Int
type TurnIsComplete = Bool

turnComplete = True
turnIncomplete = False

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
    
    grabCard :: Card -> a -> a
    
    hasBlackjack :: a -> Bool
    hasBlackjack player = getPoints player == 21 &&
                          numCards player == 2
      where
        numCards = length . getCards . getHand 

    busts :: a -> Bool
    busts = (>21) . getPoints

    isDealer :: a -> Bool

    isPlayer :: a -> Bool
    isPlayer = not . isDealer

instance CardPlayer Dealer where
    getHand (Dealer hand) = hand
    
    viewHand False = Hand . tail . getCards . getHand
    viewHand True = getHand
    
    grabCard card (Dealer (Hand hand)) = Dealer $ Hand (card:hand)

    isDealer _ = True

instance CardPlayer Player where
    getHand (Player hand _) = hand
    
    viewHand _ = getHand
    
    grabCard card (Player (Hand hand) cash) = Player (Hand (card:hand)) cash

    isDealer _ = False

