module HackJack (main) where

import Data.Char (toLower)
import System.Console.ANSI (clearScreen)

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
    return ()
    
type InputString = String
type OutputString = String
data GoodOrBad a = Good a | Bad a deriving (Eq, Read, Show)

interactGoodOrBad :: (InputString -> GoodOrBad OutputString) -> IO ()
interactGoodOrBad f = do
    input <- getLine
    process . f $ input
  where
    process (Good output) = putStrLn output
    process (Bad output) = putStrLn output >> interactGoodOrBad f

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
    \   Welcome to HackJack!\n\
    \**************************\n"

rules :: String
rules = "\n\
    \Goal is to get as close to 21 without going over.\n\
    \The player with the most points wins.\n\
    \Cards 2-9 are worth their face value.\n\
    \The 10 and face cards (J, Q, K) are worth 10 points.\n\
    \Aces are worth 11 or 1, whichever is most favorable\n\
    \to the player.\n"
