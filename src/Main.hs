module Main (main) where

import Deck (newDeck)
import Players (newDealer, newPlayer)

main :: IO ()
main = do
    deck <- newDeck
    let player = newPlayer
    let dealer = newDealer
    return ()

