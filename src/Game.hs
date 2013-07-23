{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Game where

import Control.Lens
import Data.Char
import Control.Monad.Trans.State

import Cards
import Deck
import Players
import Helpers

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

showRules :: InputString -> GoodOrBad OutputString
showRules input = processResponse
  where
    response = map toLower input
    processResponse
        | response `elem` ["y", "yes", "yeah", "sure"] =
            Good rules
        | response `elem` ["n", "no", "nope", "nay"] =
            Good "Ok, let's get started!\n"
        | otherwise =
            Bad "I'm sorry, I didn't understand.\n\
                \Please say either yes or no.  "

title :: String
title = "\n\
    \**************************\n\
    \   Welcome to Hackjack!\n\
    \**************************\n"

rules :: String
rules = "\n\
    \Goal is to get as close to 21 without going over.\n\
    \The player with the most points wins.\n\
    \Cards 2-9 are worth their face value.\n\
    \The 10 and face cards (J, Q, K) are worth 10 points.\n\
    \Aces are worth 11 or 1, whichever is most favorable\n\
    \to the player.\n"

showTable :: Dealer -> Player -> TurnIsComplete -> String
showTable d p t =
    "Cash: $" ++ (show . getCash) p ++ "\n\
    \n\
    \Your hand:      " ++ (show . viewHand t) p ++ "\
    \Dealer's hand:  " ++ (show . viewHand t) d ++ "\n\
    \\n\
    \You have " ++ (show . getPoints . viewHand t) p ++ ",\n\
    \dealer is showing " ++ (show . getPoints . viewHand t) d ++ "\n"

