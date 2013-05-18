module Hackjack (main) where

import Deck (newDeck)
import Game (title, showRules)
import Helpers (interactGoodOrBad)

main :: IO ()
main = do
    intro
    
intro :: IO ()
intro = do
    putStrLn title
    putStrLn "Would you like me to explain the rules?"
    interactGoodOrBad showRules
    putStrLn "Press ENTER to continue"
    _ <- getLine
    return ()

round :: IO ()
round = do
    
