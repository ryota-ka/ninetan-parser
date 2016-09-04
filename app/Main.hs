module Main where

import Web.Ninetan (Forecast(..), getCurrentYearInJST, Location(..), Ninetan(..), parseNinetan)
import Data.List (intercalate)
import Data.Time.LocalTime (localDay, localTimeOfDay)

main :: IO ()
main = do
    Ninetan (Location prefecture place) forecasts <- either (const fail) id <$> (parseNinetan <$> getCurrentYearInJST <*> getContents)
    putStrLn prefecture
    putStrLn place
    mapM_ (putStrLn . formatForecast) forecasts
    where fail = error "Parse error"
          formatForecast (Forecast t a b c d e f g) = intercalate " " $ show (localDay t) : show (localTimeOfDay t) : map show [a, b, c, d, e, f, g]
