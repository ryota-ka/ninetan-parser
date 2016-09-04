module Web.Ninetan (
    Forecast(..)
  , getCurrentYearInJST
  , Location(..)
  , Ninetan(..)
  , parseNinetan
  ) where

import Data.Time (getCurrentTime, localDay, TimeZone(..), utcToLocalTime)
import Data.Time.Calendar (toGregorian)
import Web.Ninetan.Parser (parseNinetan)
import Web.Ninetan.Types (Forecast(..), Location(..), Ninetan(..))

getCurrentYearInJST :: IO Integer
getCurrentYearInJST = first . toGregorian . localDay . utcToLocalTime jst <$> getCurrentTime
    where first (x, _, _) = x
          jst = TimeZone 540 False "JST"
