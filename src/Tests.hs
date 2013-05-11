{-# LANGUAGE TemplateHaskell #-}
module Tests (main) where

import Test.QuickCheck
import Test.QuickCheck.All

import Cards (Card(..), Rank(..), Suit(..), Hand(..), allCards, getPoints)
import Deck (baseDeck, dealCard, shouldReshuffle)
import Players

prop_weHave52Cards = length allCards == 52

prop_getPoints_Card = getPoints card == 10
  where
    card = Card King Spades

prop_getPoints_BasicHand = getPoints hand == 21
  where
    hand = Hand [Card King Spades, 
                 Card Two Hearts,
                 Card Five Clubs,
                 Card Four Diamonds]

prop_getPoints_9_plus_Ace = getPoints hand == 20
  where
    hand = Hand [Card Nine Clubs,
                     Card Ace Diamonds]

prop_getPoints_20_plus_Ace = getPoints hand == 21
  where 
    hand = Hand [Card King Spades,
                 Card Jack Diamonds,
                 Card Ace Spades]

prop_getPoints_2_Aces = getPoints hand == 12
  where
    hand = Hand [Card Ace Spades,
                 Card Ace Diamonds]

prop_getPoints_8_plus_3_Aces = getPoints hand == 21
  where
    hand = Hand [Card Ace Spades,
                 Card Ace Diamonds,
                 Card Ace Clubs,
                 Card Eight Hearts]

prop_dealCard_pure = getCards hand == [head baseDeck]
                  && length deck == length baseDeck - 1
  where
    (deck, hand) = dealCard baseDeck emptyHand
    emptyHand = Hand []

prop_shouldReshuffleFalse = not . shouldReshuffle $ baseDeck

prop_shouldReshuffle = shouldReshuffle deck
  where
    deck = take (length baseDeck `div` 2 - 1) baseDeck

prop_Dealer_viewHand_TurnIsNotComplete =
    length (getCards (viewHand dealer False)) == 1
  where
    dealer = Dealer $ Hand $ take 2 baseDeck

prop_Dealer_viewHand_TurnIsComplete =
    length (getCards (viewHand dealer True)) == 2
  where
    dealer = Dealer $ Hand $ take 2 baseDeck

main = $(quickCheckAll)

