module Web.Ninetan.Types (
    Forecast(..)
  , Location(..)
  , Ninetan(..)
  ) where

import Data.Time (LocalTime)

data Ninetan = Ninetan {
    location  :: Location
  , forecasts :: [Forecast]
  } deriving (Show)

data Forecast  = Forecast {
    time    :: LocalTime
  , pp01mm  :: Integer
  , pp1mm   :: Integer
  , pp5mm   :: Integer
  , pp10mm  :: Integer
  , pp20mm  :: Integer
  , pp50mm  :: Integer
  , pp100mm :: Integer
  } deriving (Show)

data Location = Location {
    prefecture :: String
  , place      :: String
  } deriving (Eq, Show)
