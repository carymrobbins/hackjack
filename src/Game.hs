module Game where

import Control.Lens
import Deck
import Players

type Bet = Int

data Game = Game
    { _dealer :: Dealer
    , _player :: Player
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
hideDealerCard game = game & (dealer.hand) %~ (take 1)

dealCard game playerLens = game
    & (playerLens.hand) %~ (c:)
    & deck .~ cs
  where
    (c:cs) = game^.deck 

