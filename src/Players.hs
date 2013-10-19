module Players where

import Cards (Hand(..), Points, handPoints)

type Cash = Int

data Dealer = Dealer
    deriving (Show)

data Player = Player { cash :: Cash }
    deriving (Show)

data CardPlayer a = CardPlayer
    { cardPlayer :: a
    , hand :: Hand
    }
    deriving (Show)

dealerHitMax :: Points
dealerHitMax = 17

newDealer :: CardPlayer Dealer
newDealer = CardPlayer
    { cardPlayer=Dealer
    , hand=[]
    }

newPlayer :: CardPlayer Player
newPlayer = CardPlayer
    { cardPlayer=Player { cash=100 }
    , hand=[]
    }

playerPoints :: CardPlayer a -> Int
playerPoints = handPoints . hand

hasBlackjack :: CardPlayer a -> Bool
hasBlackjack p = has21Points && has2Cards
  where
    has21Points = (== 21) . handPoints . hand $ p
    has2Cards = (== 2) . length . hand $ p

busts :: CardPlayer a -> Bool
busts = (> 21) . handPoints . hand

setHand :: CardPlayer a -> Hand -> CardPlayer a
setHand p h = p { hand=h }

modHand :: CardPlayer a -> (Hand -> Hand) -> CardPlayer a
modHand p f = setHand p . f . hand $ p

setCash :: CardPlayer Player -> Cash -> CardPlayer Player
setCash p c = p { cardPlayer=Player { cash=c } }

modCash :: CardPlayer Player -> (Cash -> Cash) -> CardPlayer Player
modCash p f = setCash p . f . cash . cardPlayer $ p

