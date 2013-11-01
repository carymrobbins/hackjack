module Players where

import Control.Lens (makeLenses)
import Cards (Hand(..), Points, handPoints)

type Cash = Int

data Dealer = Dealer
    deriving (Show)

data Player = Player { _cash :: Cash }
    deriving (Show)

makeLenses ''Player

data CardPlayer a = CardPlayer
    { _cardPlayer :: a
    , _hand :: Hand
    }
    deriving (Show)

makeLenses ''CardPlayer

dealerHitMax :: Points
dealerHitMax = 17

newDealer :: CardPlayer Dealer
newDealer = CardPlayer
    { _cardPlayer=Dealer
    , _hand=[]
    }

newPlayer :: CardPlayer Player
newPlayer = CardPlayer
    { _cardPlayer=Player { _cash=100 }
    , _hand=[]
    }

playerPoints :: CardPlayer a -> Int
playerPoints = handPoints . _hand

hasBlackjack :: CardPlayer a -> Bool
hasBlackjack p = has21Points && has2Cards
  where
    has21Points = (== 21) . handPoints . _hand $ p
    has2Cards = (== 2) . length . _hand $ p

busts :: CardPlayer a -> Bool
busts = (> 21) . handPoints . _hand

setHand :: CardPlayer a -> Hand -> CardPlayer a
setHand p h = p { _hand=h }

modHand :: CardPlayer a -> (Hand -> Hand) -> CardPlayer a
modHand p f = setHand p . f . _hand $ p

setCash :: CardPlayer Player -> Cash -> CardPlayer Player
setCash p c = p { _cardPlayer=Player { _cash=c } }

modCash :: CardPlayer Player -> (Cash -> Cash) -> CardPlayer Player
modCash p f = setCash p . f . _cash . _cardPlayer $ p

