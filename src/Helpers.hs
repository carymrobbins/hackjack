module Helpers where

import System.IO (hFlush, stdout)

maybeRead :: Read a => String -> Maybe a
maybeRead s = extract $ ( reads s :: Read a => [(a, String)] )
  where
    extract [] = Nothing
    extract value = Just . fst . head $ value

clearScreen :: IO ()
clearScreen = putStrLn "\ESC[2J" >> putStrLn "\ESC[H"

waitForEnter :: IO ()
waitForEnter = putStrLn "\nPress enter to continue." >> getLine >> return ()

prompt :: String -> IO String
prompt s = putStr s >> hFlush stdout >> getLine

