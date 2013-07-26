module Helpers where

maybeRead :: Read a => String -> Maybe a
maybeRead s = extract $ ( reads s :: Read a => [(a, String)] )
  where
    extract [] = Nothing
    extract value = Just . fst . head $ value

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

