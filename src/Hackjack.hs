module Main (main) where

import Control.Lens
import Control.Monad (liftM)
import Control.Monad.Trans.State (execState)
import Data.Char (toLower)
import Text.Printf

import Cards (getPoints, showHand)
import Deck (newDeck)
import Game (Game, _dealer, dealer, initHands, newGame, _player, player)
import Helpers 
import Players (TurnIsComplete, turnIncomplete, turnComplete, viewHand)

main :: IO ()
main = do
    intro
    
intro :: IO ()
intro = do
    putStrLn title
    putStrLn "Would you like me to explain the rules?"
    interactGoodOrBad showRules
    putStrLn "Press ENTER to continue"
    getLine
    game <- newGame
    let g = execState initHands game
    putStrLn $ showGame g turnIncomplete
    putStrLn "Press ENTER to continue"
    getLine
    return ()

showGame :: Game -> TurnIsComplete -> String
showGame game turn =
    showGameBase
        (show dealerHand)
        (getPoints dealerHand)
        (show playerHand)
        (getPoints playerHand)
  where
    dealerHand = game^.dealer.to (viewHand turn)
    playerHand = game^.player.to (viewHand turn)

showGameBase :: String -> Int -> String -> Int -> String
showGameBase dealerHand dealerPoints playerHand playerPoints =
    printf
        "Dealer: %s\n\
        \        showing %d\n\
        \Player: %s\n\
        \        showing %d\n"
        dealerHand
        dealerPoints
        playerHand
        playerPoints

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

