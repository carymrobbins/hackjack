module Game where

import Deck (Deck(..), newDeck)
import Players (CardPlayer(..), Dealer(..), Player(..),
                newDealer, newPlayer)

data IOState = NewGame | GetBet | PlayerMove
    deriving (Show)

data PureState = StartGame | InitialDeal
    deriving (Show)

type Bet = Int

data Game = Game
    { dealer :: CardPlayer Dealer
    , player :: CardPlayer Player
    , deck :: Deck
    , bet :: Bet
    }
    deriving (Show)

newGame :: IO Game
newGame = do
    d <- newDeck
    return Game
        { dealer=newDealer
        , player=newPlayer
        , deck=d
        , bet=0
        }

