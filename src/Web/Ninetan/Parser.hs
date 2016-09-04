module Web.Ninetan.Parser (
    parseNinetan
  ) where

import Control.Applicative ((<|>), many)
import Data.Time (TimeOfDay(..))
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (LocalTime(..))
import Text.Parsec (parse)
import Text.Parsec.Error (ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, lower, string)
import Text.Parsec.Combinator (between, eof, many1)
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (javaStyle)
import Web.Ninetan.Types (Forecast(..), Location(..), Ninetan(..))

lexer = P.makeTokenParser javaStyle

comma = P.comma lexer
dot = P.dot lexer
natural = P.natural lexer
parens = P.parens lexer
semi = P.semi lexer
symbol = P.symbol lexer


quoted :: Parser a -> Parser a
quoted = between (symbol "'") (symbol "'")

skipDataSetValue :: Parser ()
skipDataSetValue = string "data" *> dot *> string "setValue" *> pure ()

prologue :: Parser Location
prologue = string "function update_" *> (Location <$> many1 lower <* char '_' <*> many1 lower <* parens (string "data") <* symbol "{")

epilogue :: Parser ()
epilogue = symbol "}" *> eof

addRowsP :: Parser ()
addRowsP = string "data" *> dot *> string "addRows" *> parens natural *> semi *> pure ()

setValueHelper :: Parser a -> Parser a
setValueHelper p = skipDataSetValue *> parens (natural *> comma *> natural *> comma *> p) <* semi

precipitationProbabilityP :: Parser Integer
precipitationProbabilityP = setValueHelper natural

timeLabelP :: Integer -> Parser LocalTime
timeLabelP year = do
    (m, d, hh, mm) <- setValueHelper $ quoted $ (,,,) <$> natural <* char '月' <*> natural <* char '日' <* char ' ' <*> natural <* char '時' <*> natural <* char '分'
    let day = fromGregorian year (fromInteger m) (fromInteger d)
        time = TimeOfDay (fromInteger hh) (fromInteger mm) 0
    pure (LocalTime day time)

forecastP :: Integer -> Parser Forecast
forecastP year = Forecast <$> timeLabelP year
                          <*> precipitationProbabilityP
                          <*> precipitationProbabilityP
                          <*> precipitationProbabilityP
                          <*> precipitationProbabilityP
                          <*> precipitationProbabilityP
                          <*> precipitationProbabilityP
                          <*> precipitationProbabilityP

ninetanP :: Integer -> Parser Ninetan
ninetanP year = Ninetan <$> prologue <* addRowsP <*> many (forecastP year) <* epilogue

parseNinetan :: Integer -> String -> Either ParseError Ninetan
parseNinetan year = parse (ninetanP year) "input source"
