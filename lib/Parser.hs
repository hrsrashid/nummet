module Parser where

import Text.Trifecta
import Data.Number.CReal
import Data.Vector
import Data.Matrix.Dense.Generic
import qualified Data.Scientific as Sci

parseInput :: Parser a -> Parser a
parseInput p = do
  skipComments
  whiteSpace
  result <- p
  whiteSpace
  skipComments
  return result

eol :: String
eol = "\n\r"

space' :: String
space' = " \t"

skipEOL :: Parser ()
skipEOL = skipMany $ oneOf eol

skipComments :: Parser ()
skipComments = skipMany $ do
  _ <- char '#'
  skipMany (noneOf eol)
  skipEOL

parseDecimal :: Parser CReal
parseDecimal = fmap (either fromInteger Sci.toRealFloat) $ runUnspaced $ do
  _ <- many (oneOf space')
  integerOrScientific

parseVector :: Parser (Vector CReal)
parseVector = fromList <$> parseDecimal `sepEndBy1` some (oneOf space')

parseVectors :: Parser [Vector CReal]
parseVectors = parseVector `sepEndBy1` some (oneOf eol)

parseMatrix :: Parser (Matrix Vector CReal)
parseMatrix = fromRows <$> parseVectors
