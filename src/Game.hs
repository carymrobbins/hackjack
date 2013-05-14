module Game where

import Players
import Deck
import Cards



--newtype GameSate s a = GameState { runGameState :: s -> (a, s) }


--newGame :: IO (Deck, TheDealer, ThePlayer)
--newGame = 

showTable dealer player turnIsComplete =
    "Cash: $" ++ getCash player ++ "\n\
    \\n\
    \Your hand:      " ++ showHand player ++ "\
    \Dealer's hand:  " ++ showHand dealer ++ "\n\
    \\n\
    \You have " ++ getPoints player ++ ",\n\
    \dealer is showing " ++ getPoints dealer ++ "\n"
  where
    showHand p = show $ viewHand p turnIsComplete 

