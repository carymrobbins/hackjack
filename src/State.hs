module State where

import Data.Char (toLower)
import Cards (handPoints)
import Deck (newDeck, shouldReshuffle)
import Game (Game(..), Bet, hideDealerCard, newGame, updateCashFromBet,
             dealCard)
import Helpers (clearScreen, maybeRead, prompt, waitForEnter)
import Players (Player(..), CardPlayer(..), modCash, modHand, setHand,
                hasBlackjack, busts, dealerHitMax, playerPoints)
import PPrint (pprint)

data IOState = NewGame | Reshuffle | GetBet | GetMove | ShowDealerHit
             | PlayerBlackjack | PlayerBusts | ShowPlayerWins
             | DealerBlackjack | DealerBusts | ShowDealerWins
             | ShowTie | FinalResults | GameOver | Quit | PlayAgain
    deriving (Show)

data PureState = StartGame | InitialDeal | CheckBlackjacks | DealerMove
               | PlayerHit | PlayerStay | CheckPlayerHit | PlayerWins
               | DealerHit | DealerStay | CheckDealerHit | DealerWins
               | FindWinner | Tie | PlayerQuit
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

handleIO PlayerBusts game = do
    clearScreen
    putStrLn "You bust, dealer wins."
    handleIO FinalResults game

handleIO ShowDealerHit game = do
    clearScreen
    putStrLn "Dealer hits.\n"
    putStrLn . pprint $ game
    waitForEnter
    return (DealerMove, game)

handleIO DealerBusts game = do
    clearScreen
    putStrLn "Dealer busts, you win!"
    handleIO FinalResults game

handleIO FinalResults game = do
    putStrLn . ('\n':) . pprint $ game
    waitForEnter
    return (StartGame, game)

handleIO PlayerBlackjack game = do
    clearScreen
    putStrLn "You got a blackjack, you win!"
    handleIO FinalResults game

handleIO DealerBlackjack game = do
    clearScreen
    putStrLn "Dealer got a blackjack, dealer wins."
    handleIO FinalResults game

handleIO ShowPlayerWins game = do
    clearScreen
    putStrLn "You win!"
    handleIO FinalResults game

handleIO ShowDealerWins game = do
    clearScreen
    putStrLn "Dealer wins."
    handleIO FinalResults game

handleIO ShowTie game = do
    clearScreen
    putStrLn "Push - it's a tie game!"
    handleIO FinalResults game

handleIO GameOver game = do
    clearScreen
    putStrLn "GAME OVER\n"
    getPlayAgain game

handleState :: PureState -> Game -> (IOState, Game)

handleState StartGame game
    | (cash . cardPlayer . player $ game) == 0 = (GameOver, game)
    | shouldReshuffle $ deck game = (Reshuffle, game)
    | otherwise = (GetBet, game)

handleState InitialDeal game = handleState CheckBlackjacks game'
  where
    ([c1, c2, c3, c4], rest) = splitAt 4 . deck $ game
    player' = modCash (player game) (subtract . bet $ game)
    game' = game
        { player=setHand player' [c1, c3]
        , dealer=setHand (dealer game) [c2, c4]
        , deck=rest
        }

handleState CheckBlackjacks game
    | hasBlackjack (player game) && hasBlackjack (dealer game) = handleState Tie game
    | hasBlackjack $ player game = let (_, game') = handleState PlayerWins game
                                   in (PlayerBlackjack, game')
    | hasBlackjack $ dealer game = let (_, game') = handleState DealerWins game
                                   in (DealerBlackjack, game')
    | otherwise = (GetMove, game)

handleState PlayerHit game = handleState CheckPlayerHit game'
  where
    (d', p') = dealCard (deck game) (player game)
    game' = game
        { player=p'
        , deck=d'
        }

handleState PlayerStay game = handleState DealerMove game

handleState CheckPlayerHit game
    | busts $ player game = (PlayerBusts, game)
    | otherwise = (GetMove, game)

handleState DealerMove game
    | (playerPoints . dealer $ game) < dealerHitMax = handleState DealerHit game
    | otherwise = handleState DealerStay game

handleState DealerHit game = handleState CheckDealerHit game'
  where
    (d', p') = dealCard (deck game) (dealer game)
    game' = game
        { dealer=p'
        , deck=d'
        }

handleState DealerStay game = handleState FindWinner game

handleState CheckDealerHit game
    | busts . dealer $ game = (DealerBusts, snd . handleState PlayerWins $ game)
    | otherwise = (ShowDealerHit, game) 

handleState FindWinner game
    | playerPoints (dealer game) > playerPoints (player game) = handleState DealerWins game
    | playerPoints (dealer game) < playerPoints (player game) = handleState PlayerWins game
    | otherwise = handleState Tie game

handleState PlayerWins game = (ShowPlayerWins, game')
  where
    game' = game
        { player=modCash (player game) (+ bet game * 2) }

handleState DealerWins game = (ShowDealerWins, game)

handleState Tie game = (ShowTie, game')
  where
    game' = game
        { player=modCash (player game) (+ bet game) }

handleState PlayerQuit game = (Quit, game)

getBet :: Game -> IO (PureState, Game)
getBet game = do
    response <- prompt "Place your bet: "
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
    response <- prompt "Would you like to hit or stay? "
    handleMoveResponse (map toLower response) game

handleMoveResponse :: String -> Game -> IO (PureState, Game)
handleMoveResponse "hit" game = return (PlayerHit, game)
handleMoveResponse "stay" game = return (PlayerStay, game)
handleMoveResponse _ game = putStrLn "Invalid move." >> getMove game

getPlayAgain :: Game -> IO (PureState, Game)
getPlayAgain game = do
    response <- prompt "Would you like to play again? "
    handlePlayAgainResponse (map toLower response) game

handlePlayAgainResponse :: String -> Game -> IO (PureState, Game)
handlePlayAgainResponse response game
    | response `elem` ["yes", "y"] = do
        game' <- newGame
        return (StartGame, game')
    | response `elem` ["no", "n"] = return (PlayerQuit, game)
    | otherwise = putStrLn "Please say yes or no." >> getPlayAgain game

