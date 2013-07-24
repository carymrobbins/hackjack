{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Game where

import Control.Lens
import Data.Char
import Control.Monad.Trans.State

import Cards
import Deck
import Players

data Game = Game
    { _deck :: Deck
    , _dealer :: Dealer
    , _player :: Player
    } deriving (Show)

makeLenses ''Game

newGame :: IO Game
newGame = do
    d <- newDeck
    return Game { _deck=d
                , _dealer=newDealer
                , _player=newPlayer }

popDeck :: State Game Card
popDeck = do
    c:d <- use deck
    deck .= d
    return c

dealCard :: CardPlayer a => Lens' Game a -> State Game ()
dealCard playerLens = do
    c <- popDeck
    playerLens.hand.cards %= (c:)

initHands :: State Game ()
initHands = do
    dealCard player
    dealCard dealer
    dealCard player
    dealCard dealer

