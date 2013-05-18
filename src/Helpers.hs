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

newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState
