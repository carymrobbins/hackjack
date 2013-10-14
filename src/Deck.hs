module Deck where

import Control.Monad (liftM)

import System.Random.Shuffle (shuffleM)

import Cards (Card, Cards, Hand(..), handCards, allCards)


data Deck = Deck { deckCards :: Cards }
            deriving (Show)

numberOfDecks :: Int
numberOfDecks = 6

baseDeck :: Deck
baseDeck = Deck . concat . take numberOfDecks . repeat $ allCards

newDeck :: IO (Deck)
newDeck = liftM Deck . shuffleM . deckCards $ baseDeck

shouldReshuffle :: Deck -> Bool
shouldReshuffle deck = 
    length (deckCards deck) < length (deckCards baseDeck) `div` 2

popDeck :: Deck -> (Deck, Card)
popDeck deck = (Deck rest, card)
  where
    (card:rest) = deckCards deck

