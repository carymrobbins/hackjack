module Helpers where

type InputString = String
type OutputString = String

data GoodOrBad a = Good a | Bad a deriving (Eq, Read, Show)

interactGoodOrBad :: (InputString -> GoodOrBad OutputString) -> IO ()
interactGoodOrBad f = do
    input <- getLine
    process . f $ input
  where
    process (Good output) = putStrLn output
    process (Bad output) = putStrLn output >> interactGoodOrBad f

