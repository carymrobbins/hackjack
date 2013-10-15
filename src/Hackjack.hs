module Main (main) where

import Control.Lens
import Control.Monad (liftM)
import Control.Monad.Trans.State (execState)
import Data.Char (toLower)
import System.IO
import Text.Printf (printf)

import Cards (getPoints, showHand)
import Deck (newDeck)
import Game
import Helpers
import Players
import PPrint

main :: IO ()
main = intro

intro :: IO ()
intro = do
    putStrLn title
    putStrLn "Would you like me to explain the rules?"
    interactGoodOrBad showRules
    game <- newGame
    bet <- promptForBet $ game^.player.cash
    game <- return $ gameRound bet `execState` game
    putStrLn $ pprint game
    return ()

promptForBet :: Cash -> IO Cash
promptForBet playerCash = do
    putStrLn $ printf "You have $%d" playerCash
    putStrLn "Please enter your bet: "
    bet <- getLine >>= (\bet ->
        if length bet > 0 && head bet == '$' then
            return $ tail bet
        else
            return bet)
    let maybeBet = maybeRead bet :: Maybe Int
    validate bet maybeBet
  where
    validate betString Nothing = do
        putStrLn $ printf "'%s' is not a number." betString
        promptForBet playerCash
    validate betString (Just betValue)
        | betValue > playerCash = do
            putStrLn "You don't have enough cash!"
            promptForBet playerCash
        | betValue `mod` 10 /= 0 = do
            putStrLn "Bet must be a multiple of 10."
            promptForBet playerCash
        | betValue == 0 = do
            putStrLn "Bet must be greater than $0."
            promptForBet playerCash
        | otherwise = return betValue

waitForEnter :: IO ()
waitForEnter = do
    putStrLn "Press ENTER to continue." 
    _ <- getLine
    return ()

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

