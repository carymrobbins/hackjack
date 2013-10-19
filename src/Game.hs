module Game where

import Deck (Deck(..), newDeck)
import Players (CardPlayer(..), Dealer(..), Player(..),
                newDealer, newPlayer, modCash)

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

hideDealerCard :: Game -> Game
hideDealerCard game = game
    { dealer=(dealer game)
        { hand=take 1 . hand . dealer $ game }
    }

updateCashFromBet :: Game -> (Int -> Int -> Int) -> Game
updateCashFromBet game op =
    game { player=modCash (player game) (flip op . bet $ game) }

