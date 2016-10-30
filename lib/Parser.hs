module Parser where

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Data.List                 as L
import           Data.Matrix.Dense.Generic
import           Data.Number.CReal
import qualified Data.Scientific           as Sci
import           Data.Vector
import           Text.Trifecta

parseFile :: MonadIO m => Parser a -> String -> m (Either String a)
parseFile p s = toEither <$> parseFromFileEx p s
  where
    toEither (Success a) = Right a
    toEither (Failure a) = Left (show a)

parseInput :: Parser a -> Parser a
parseInput p = skipJunk *> p

skipJunk :: Parser ()
skipJunk = skipMany $ some space <|> comment

eol :: String
eol = "\n\r"

space' :: String
space' = " \t"

comment :: Parser String
comment = do
  _ <- char '#'
  some (noneOf eol)

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
