module State where

import Control.Lens
import Data.Char (toLower)
import Cards
import Deck
import Game
import Helpers
import Players
import PPrint

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
    putStrLn $ "Cash $" ++ (game^.player.cardPlayer.cash.to show)
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
    | game^.player.cardPlayer.cash.to (== 0) = (GameOver, game)
    | game^.deck.to shouldReshuffle = (Reshuffle, game)
    | otherwise = (GetBet, game)

handleState InitialDeal game = handleState CheckBlackjacks game'
  where
    ([c1, c2, c3, c4], rest) = game^.deck.to (splitAt 4)
    game' = game
        & (player.cardPlayer.cash) -~ (game^.bet)
        & (player.hand) .~ [c1, c3]
        & (dealer.hand) .~ [c2, c4]
        & deck .~ rest

handleState CheckBlackjacks game
    | hasBlackjack (game^.player) && hasBlackjack (game^.dealer) =
        handleState Tie game
    | hasBlackjack $ game^.player =
        let (_, game') = handleState PlayerWins game
        in (PlayerBlackjack, game')
    | hasBlackjack $ game^.dealer =
        let (_, game') = handleState DealerWins game
        in (DealerBlackjack, game')
    | otherwise = (GetMove, game)

handleState PlayerHit game = handleState CheckPlayerHit game'
  where
    (d', p') = dealCard (game^.deck) (game^.player)
    game' = game
        { _player=p'
        , _deck=d'
        }

handleState PlayerStay game = handleState DealerMove game

handleState CheckPlayerHit game
    | busts $ game^.player = (PlayerBusts, game)
    | otherwise = (GetMove, game)

handleState DealerMove game
    | (playerPoints . _dealer $ game) < dealerHitMax = handleState DealerHit game
    | otherwise = handleState DealerStay game

handleState DealerHit game = handleState CheckDealerHit game'
  where
    (d', p') = dealCard (_deck game) (_dealer game)
    game' = game
        { _dealer=p'
        , _deck=d'
        }

handleState DealerStay game = handleState FindWinner game

handleState CheckDealerHit game
    | busts . _dealer $ game = (DealerBusts, snd . handleState PlayerWins $ game)
    | otherwise = (ShowDealerHit, game) 

handleState FindWinner game
    | playerPoints (_dealer game) > playerPoints (_player game) = handleState DealerWins game
    | playerPoints (_dealer game) < playerPoints (_player game) = handleState PlayerWins game
    | otherwise = handleState Tie game

handleState PlayerWins game = (ShowPlayerWins, game')
  where
    game' = game
        { _player=modCash (_player game) (+ _bet game * 2) }

handleState DealerWins game = (ShowDealerWins, game)

handleState Tie game = (ShowTie, game')
  where
    game' = game
        { _player=modCash (_player game) (+ _bet game) }

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
    | b > (game^.player.cardPlayer.cash) = do
        putStrLn "You cannot bet more than you have!"
        getBet game
    | otherwise = return (InitialDeal, game { _bet=b })

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

