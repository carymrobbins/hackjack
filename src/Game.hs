module Game where

import Control.Lens (makeLenses)
import Deck (Deck(..), newDeck)
import Players (CardPlayer(..), Dealer(..), Player(..),
                newDealer, newPlayer, modCash, modHand)

type Bet = Int

data Game = Game
    { _dealer :: CardPlayer Dealer
    , _player :: CardPlayer Player
    , _deck :: Deck
    , _bet :: Bet
    }
    deriving (Show)

makeLenses ''Game

baseGame :: Game
baseGame = Game
    { _dealer=newDealer
    , _player=newPlayer
    , _deck=[]
    , _bet=0
    }

newGame :: IO Game
newGame = do
    d <- newDeck
    return baseGame { _deck=d }

hideDealerCard :: Game -> Game
hideDealerCard game = game
    { _dealer=(_dealer game)
        { _hand=take 1 . _hand . _dealer $ game }
    }

updateCashFromBet :: Game -> (Int -> Int -> Int) -> Game
updateCashFromBet game op =
    game { _player=modCash (_player game) (flip op . _bet $ game) }

dealCard :: Deck -> CardPlayer a -> (Deck, CardPlayer a)
dealCard d p = (d', p')
  where
    (c:d') = d
    p' = modHand p (c:)

