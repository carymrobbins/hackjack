module Deck where

import System.Random.Shuffle (shuffleM)

import Cards (Cards, Hand(..), _cards, allCards)


type Deck = Cards

numberOfDecks :: Int
numberOfDecks = 6

baseDeck :: Deck
baseDeck = concat . take numberOfDecks $ repeat allCards

newDeck :: IO (Deck)
newDeck = shuffleM baseDeck

shouldReshuffle :: Deck -> Bool
shouldReshuffle deck = length deck < length baseDeck `div` 2

