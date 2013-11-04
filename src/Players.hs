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

