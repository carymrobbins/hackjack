module Players where

import Cards

type Cash = Int
type TurnIsComplete = Bool

data CardPlayer = Dealer Hand | Player Hand Cash

instance HasPoints CardPlayer where
    getPoints = getPoints . getHand

getCash :: CardPlayer -> Maybe Cash
getCash (Dealer _) = Nothing
getCash (Player _ cash) = Just cash

getHand :: CardPlayer -> Hand
getHand (Dealer hand) = hand
getHand (Player hand _) = hand

viewHand :: CardPlayer -> TurnIsComplete -> Hand
viewHand (Dealer hand) False = Hand . tail $ getCards hand
viewHand (Dealer hand) True = hand
viewHand (Player hand _) _ = hand

hasBlackjack :: CardPlayer -> Bool
hasBlackjack player = (getPoints . getHand) player == 21 &&
                      (length . getCards . getHand) player == 2

busts :: CardPlayer -> Bool
busts player = (getPoints . getHand) player > 21

