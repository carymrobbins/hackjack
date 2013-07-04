module Game where

import Data.Char

import Cards
import Deck
import Players
import Helpers

data GameData = GameData { deck :: Deck
                         , dealer :: Dealer
                         , player :: Player }
                deriving (Show)

type GameState a = State GameData a

newGameData :: IO GameData
newGameData = do
    d <- newDeck
    return GameData { deck=d
                    , dealer=newDealer
                    , player=newPlayer }

drawCard :: CardPlayer b => (GameData -> b) -> GameState ()
drawCard cardPlayer =
    State $ \gameData -> let (card:cards) = deck gameData
                             thePlayer = let p = player gameData
                                         in if isPlayer $ cardPlayer gameData 
                                            then grabCard p card
                                            else p
                             theDealer = let d = dealer gameData
                                         in if isDealer $ cardPlayer gameData
                                            then grabCard d card
                                            else d
                         in ((), GameData { deck=cards
                                          , dealer=theDealer
                                          , player=thePlayer })

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
