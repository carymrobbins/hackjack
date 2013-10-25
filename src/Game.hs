module Game where

import Deck (Deck(..), newDeck)
import Players (CardPlayer(..), Dealer(..), Player(..),
                newDealer, newPlayer, modCash, modHand)

type Bet = Int

data Game = Game
    { dealer :: CardPlayer Dealer
    , player :: CardPlayer Player
    , deck :: Deck
    , bet :: Bet
    }
    deriving (Show)

baseGame :: Game
baseGame = Game
    { dealer=newDealer
    , player=newPlayer
    , deck=[]
    , bet=0
    }

newGame :: IO Game
newGame = do
    d <- newDeck
    return baseGame { deck=d }

hideDealerCard :: Game -> Game
hideDealerCard game = game
    { dealer=(dealer game)
        { hand=take 1 . hand . dealer $ game }
    }

updateCashFromBet :: Game -> (Int -> Int -> Int) -> Game
updateCashFromBet game op =
    game { player=modCash (player game) (flip op . bet $ game) }

dealCard :: Deck -> CardPlayer a -> (Deck, CardPlayer a)
dealCard d p = (d', p')
  where
    (c:d') = d
    p' = modHand p (c:)

