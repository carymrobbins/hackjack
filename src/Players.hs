module Players where

import Cards (Card, Hand(..), Points, handPoints)
import Deck (Deck, popDeck)

type Cash = Int

data Dealer = Dealer Hand deriving (Show)
data Player = Player Hand Cash deriving (Show)

newDealer :: Dealer
newDealer = Dealer $ Hand []

newPlayer :: Player
newPlayer = Player (Hand []) 100

getCash :: Player -> Cash
getCash (Player _ c) = c

setCash :: Player -> Cash -> Player
setCash (Player h _) c = Player h c

modCash :: Player -> (Cash -> Cash) -> Player
modCash p f = setCash p . f . getCash $ p

class CardPlayer a where
    getHand :: a -> Hand
    
    setHand :: a -> Hand -> a

    pushCard :: a -> Card -> a
    pushCard p c = setHand p . Hand . (c:) . handCards . getHand $ p
    
    drawCard :: a -> Deck -> (a, Deck)
    drawCard p d = (pushCard p card, newDeck) 
      where
        (newDeck, card) = popDeck d

    playerPoints :: a -> Points
    playerPoints = handPoints . getHand

    hasBlackjack :: a -> Bool
    hasBlackjack player = playerPoints player == 21 &&
                          numCards player == 2
      where
        numCards = length . handCards . getHand 

    busts :: a -> Bool
    busts = (>21) . playerPoints

instance CardPlayer Dealer where
    getHand (Dealer h) = h

    setHand _ h = Dealer h
    
instance CardPlayer Player where
    getHand (Player h _) = h
    
    setHand (Player _ c) h = Player h c

