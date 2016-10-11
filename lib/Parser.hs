module Parser where

import Control.Monad.IO.Class
import Control.Applicative
import Text.Trifecta
import qualified Data.List as L
import Data.Number.CReal
import Data.Vector
import Data.Matrix.Dense.Generic
import qualified Data.Scientific as Sci

parseFile :: MonadIO m => Parser a -> String -> m (Either ErrInfo a)
parseFile p s = toEither <$> parseFromFileEx p s
  where
    toEither (Success a) = Right a
    toEither (Failure a) = Left a

parseInput :: Parser a -> Parser a
parseInput p = skipJunk *> p

skipJunk :: Parser ()
skipJunk = skipMany $ many space <|> comments

eol :: String
eol = "\n\r"

space' :: String
space' = " \t"

comments :: Parser String
comments = fmap L.concat $ many $ do
  skipSome (char '#')
  result <- many (noneOf eol)
  skipMany (oneOf eol)
  return result

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
