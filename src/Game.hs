module Game where

import Data.Char
import Control.Monad.Trans.State

import Cards
import Deck
import Players
import Helpers

data Game = Game { deck :: Deck
                 , dealer :: Dealer
                 , player :: Player }
                 deriving (Show)

setDeck :: Deck -> Game -> Game
setDeck deck game =
    Game { deck = deck
         , dealer = dealer game
         , player = player game }

setDealer :: Dealer -> Game -> Game
setDealer dealer game =
    Game { deck = deck game
         , dealer = dealer
         , player = player game }

setPlayer :: Player -> Game -> Game
setPlayer player game =
    Game { deck = deck game
         , dealer = dealer game
         , player = player }

newGame :: IO Game
newGame = do
    d <- newDeck
    return Game { deck=d
                , dealer=newDealer
                , player=newPlayer }

type GameState = State Game

drawCard :: (CardPlayer a) => (Game -> a) -> GameState ()
drawCard cardPlayer =
    state $ \game ->
        let (card:cards) = deck game
            set setPerson person =
                let updatedPerson = grabCard card $ person game
                 in setDeck cards $ setPerson updatedPerson game
            updatedGame =
                if isPlayer $ cardPlayer game then
                    set setPlayer player
                else
                    set setDealer dealer
         in ((), updatedGame)

roundInit :: GameState ()
roundInit = do
    drawCard player
    drawCard dealer
    drawCard player
    drawCard dealer

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

