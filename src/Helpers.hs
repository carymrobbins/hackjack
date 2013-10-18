module Helpers where

maybeRead :: Read a => String -> Maybe a
maybeRead s = extract $ ( reads s :: Read a => [(a, String)] )
  where
    extract [] = Nothing
    extract value = Just . fst . head $ value

