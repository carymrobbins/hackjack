{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Game where

import Control.Applicative
import Control.Lens
import Control.Monad.Trans.State (State)
import Data.Char

import Cards (Card, cards)
import Deck (Deck, newDeck)
import Players

data Move = Hit | Stay
            deriving (Show, Enum)

moveMap :: [(String, Move)]
moveMap = [ (f m, m) | m <- [Hit ..], f <- [lowerShow, firstShow] ]
  where
     lowerShow = map toLower . show
     firstShow = pure . head . lowerShow

data Game = Game
    { _deck :: Deck
    , _dealer :: Dealer
    , _player :: Player
    , _turnComplete :: Bool
    } deriving (Show)

makeLenses ''Game

newGame :: IO Game
newGame = do
    d <- newDeck
    return Game { _deck=d
                , _dealer=newDealer
                , _player=newPlayer 
                , _turnComplete=False
                }

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

gameRound :: Cash -> State Game ()
gameRound bet = do
    player.cash -= bet
    initHands



viewGame :: Game -> Game
viewGame g = g { _dealer=Dealer visibleHand }
  where
    visibleHand = viewHand (g^.turnComplete) (g^.dealer)

