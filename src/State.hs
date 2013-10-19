module State where

import Data.Char (toLower)
import Cards (handPoints)
import Deck (newDeck, shouldReshuffle)
import Game (Game(..), Bet, hideDealerCard, updateCashFromBet)
import Helpers (clearScreen, maybeRead, waitForEnter)
import Players (Player(..), CardPlayer(..), modCash, modHand, setHand,
                hasBlackjack, busts, dealerHitMax)
import PPrint (pprint)

data IOState = NewGame | Reshuffle | GetBet | GetMove | DealerHit
             | PlayerBlackjack | PlayerBusts
             | DealerBlackjack | DealerBusts
             | FinalResults
    deriving (Show)

data PureState = StartGame | InitialDeal | PlayerHit | PlayerStay | DealerMove
               | CheckPlayer | PlayerWins | CheckDealer | DealerWins | Tie
    deriving (Show)

handleIO :: IOState -> Game -> IO (PureState, Game)

handleIO NewGame game = do
    clearScreen
    putStrLn "Welcome to Hackjack!"
    waitForEnter
    return (StartGame, game)

handleIO Reshuffle game = do
    clearScreen
    putStrLn "Time to shuffle!"
    waitForEnter
    d <- newDeck
    handleIO GetBet game

handleIO GetBet game = do
    clearScreen
    putStrLn $ "Cash $" ++ (show . cash . cardPlayer . player $ game)
    getBet game

handleIO GetMove game = do
    clearScreen
    putStrLn . pprint . hideDealerCard $ game
    getMove game

handleIO PlayerBlackjack game = do
    clearScreen
    putStrLn "You got a blackjack, you win!"
    return (PlayerWins, game)

handleIO PlayerBusts game = do
    clearScreen
    putStrLn "You bust, dealer wins."
    return (DealerWins, game)

handleIO FinalResults game = do
    putStrLn . pprint $ game
    waitForEnter
    return (StartGame, game)

handleState :: PureState -> Game -> (IOState, Game)

handleState StartGame game
    | shouldReshuffle $ deck game = (Reshuffle, game)
    | otherwise = (GetBet, game)

handleState InitialDeal game = (GetMove, game')
  where
    ([c1, c2, c3, c4], rest) = splitAt 4 . deck $ game
    player' = modCash (player game) (subtract . bet $ game)
    game' = game
        { player=setHand player' [c1, c3]
        , dealer=setHand (dealer game) [c2, c4]
        , deck=rest
        }

handleState PlayerHit game = handleState CheckPlayer game'
  where
    (c:rest) = deck game
    game' = game
        { player=modHand (player game) (c:)
        , deck=rest
        }

handleState PlayerStay game = handleState DealerMove game

handleState CheckPlayer game
    | hasBlackjack (player game) && hasBlackjack (dealer game) = handleState Tie game
    | hasBlackjack $ player game = (PlayerBlackjack, game)
    | hasBlackjack $ dealer game = (DealerBlackjack, game)
    | busts $ player game = (PlayerBusts, game)
    | otherwise = (GetMove, game)

handleState DealerMove game
    | (handPoints . hand . dealer $ game) < dealerHitMax = undefined
    | otherwise = undefined
    
handleState PlayerWins game = (FinalResults, updateCashFromBet game (+))

handleState DealerWins game = (FinalResults, updateCashFromBet game (-))

getBet :: Game -> IO (PureState, Game)
getBet game = do
    putStr "Place your bet: "
    response <- getLine
    handleBetResponse (maybeRead response) game

handleBetResponse :: Maybe Bet -> Game -> IO (PureState, Game)
handleBetResponse Nothing game = putStrLn "Invalid bet." >> getBet game
handleBetResponse (Just b) game
    | b <= 0 || b `mod` 10 /= 0 = do
        putStrLn "Bet must be a multiple of 10."
        getBet game
    | b > (cash . cardPlayer . player) game = do
        putStrLn "You cannot bet more than you have!"
        getBet game
    | otherwise = return (InitialDeal, game { bet=b })

getMove :: Game -> IO (PureState, Game)
getMove game = do
    putStr "Would you like to hit or stay? "
    response <- getLine
    handleMoveResponse (map toLower response) game

handleMoveResponse :: String -> Game -> IO (PureState, Game)
handleMoveResponse "hit" game = return (PlayerHit, game)
handleMoveResponse "stay" game = return (PlayerStay, game)
handleMoveResponse _ game = putStrLn "Invalid move." >> getMove game

