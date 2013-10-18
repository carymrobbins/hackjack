module Deck where

import Control.Monad (liftM)

import System.Random.Shuffle (shuffleM)

import Cards (Card, Hand(..), allCards)

type Deck = [Card]

numberOfDecks :: Int
numberOfDecks = 6

baseDeck :: Deck
baseDeck = concat . take numberOfDecks . repeat $ allCards

newDeck :: IO (Deck)
newDeck = shuffleM baseDeck

shouldReshuffle :: Deck -> Bool
shouldReshuffle deck = 
    length deck < length baseDeck `div` 2

