module Main (main) where

import Data.List (splitAt)
import Game (Game, newGame)
import State (IOState(..), handleIO, handleState)

main :: IO ()
main = newGame >>= mainLoop NewGame >> return ()

mainLoop :: IOState -> Game -> IO (IOState, Game)
mainLoop state game = 
    handleIO state game >>=
    return . uncurry handleState >>=
    uncurry mainLoop

