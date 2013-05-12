module Players where

import Control.Applicative ((<*>), pure)

import Cards

type Cash = Int
type TurnIsComplete = Bool

data Dealer = Dealer Hand
data Player = Player Hand Cash

class CardPlayer a where
    getHand :: a -> Hand
    
    viewHand :: TurnIsComplete -> a -> Hand
    
    playerPoints :: a -> Points
    playerPoints = getPoints . getHand
    
    hasBlackjack :: a -> Bool
    hasBlackjack = and . ((<*>) [ (==21) . playerPoints
                                , (==2) . length . getCards . getHand
                                ]) . pure
    
    busts :: a -> Bool
    busts = (>21) . playerPoints

instance CardPlayer Dealer where
    getHand (Dealer hand) = hand
    
    viewHand False = Hand . tail . getCards . getHand
    viewHand True = getHand

instance CardPlayer Player where
    getHand (Player hand _) = hand
    
    viewHand _ = getHand
