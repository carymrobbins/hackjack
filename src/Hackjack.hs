module Main (main) where

import Control.Monad (liftM)
import Control.Monad.Trans.State (execState)
import Text.Printf

import Cards (getPoints, showHand)
import Deck (newDeck)
import Game (Game, dealer, newGame, player, roundInit, showRules, title)
import Helpers (interactGoodOrBad)
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
    game <- liftM (execState roundInit) newGame
    putStrLn $ showGame game turnIncomplete
    return ()

showGame :: Game -> TurnIsComplete -> String
showGame game turn =
    showGameBase
        (showHand dealerHand)
        (getPoints dealerHand)
        (showHand playerHand)
        (getPoints playerHand)
  where
    hand person = viewHand turn $ person game
    --showPoints = show . getPoints
    dealerHand = hand dealer
    playerHand = hand player

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


