module Main (main) where

import Data.List (splitAt)
import Game (IOState(..), PureState(..), Game(..), Bet, newGame)
import Helpers (maybeRead)
import Players (setHand)

main :: IO ()
main = newGame >>= mainLoop NewGame >> return ()

mainLoop :: IOState -> Game -> IO (IOState, Game)
mainLoop state game = 
    handleIO state game >>=
    return . uncurry handleState >>=
    uncurry mainLoop

handleIO :: IOState -> Game -> IO (PureState, Game)

handleIO NewGame game = do
    putStrLn "Welcome to Hackjack!"
    return (StartGame, game)

handleIO GetBet game = do
    putStr "Place your bet: "
    response <- getLine
    handleBet (maybeRead response) game

handleState :: PureState -> Game -> (IOState, Game)

handleState StartGame game = (GetBet, game)

handleState InitialDeal game = (PlayerMove, game')
  where
    ([c1, c2, c3, c4], rest) = splitAt 4 . deck $ game
    game' = game
        { player=setHand (player game) [c1, c3]
        , dealer=setHand (dealer game) [c2, c4]
        }

handleBet :: Maybe Bet -> Game -> IO (PureState, Game)
handleBet Nothing game = handleIO GetBet game
handleBet (Just b) game = return (InitialDeal, game { bet=b})

