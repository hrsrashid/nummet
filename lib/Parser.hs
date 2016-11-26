module Parser where

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Data.List                 as L
import           Data.Matrix.Dense.Generic
import           Data.Number.CReal
import qualified Data.Scientific           as Sci
import           Data.Vector
import           Text.Trifecta
import qualified Library                   as Lib

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

parseVector :: Parser a -> Parser (Vector a)
parseVector p = fromList <$> p `sepEndBy1` some (oneOf space')

parseVectors :: Parser a -> Parser [Vector a]
parseVectors p = parseVector p `sepEndBy1` some (oneOf eol)

parseMatrix :: Parser a -> Parser (Matrix Vector a)
parseMatrix p = fromRows <$> parseVectors p

parseFunction :: Parser Lib.Function
parseFunction = choice
  [ parseConst
  , parseX
  , parseSin
  , parseLog
  , parseExp
  ]

parseConst :: Parser Lib.Function
parseConst = Lib.Function . const <$> parseDecimal

parseX :: Parser Lib.Function
parseX = do
  char 'x'
  index <- natural
  return $ Lib.Function (Data.Vector.! fromIntegral index)

parseSin :: Parser Lib.Function
parseSin = string "sin" >> pure (Lib.simpleFunc sin)

parseLog :: Parser Lib.Function
parseLog = string "log" >> pure (Lib.simpleFunc log)

parseExp :: Parser Lib.Function
parseExp = string "exp" >> pure (Lib.simpleFunc exp)


operators = "+-*/"

type Operator = CReal -> CReal -> CReal

parseOperator :: Parser Operator
parseOperator = do
  op <- oneOf operators
  case op of
    '+' -> return (+)
    '-' -> return (-)
    '*' -> return (*)
    '/' -> return (/)

-- operators, function compositions ...
parseExpression :: Parser Lib.Function
parseExpression = do
  f <- parseFunction
  expr <- optional (char '(' *> parseExpression <* char ')')

  let g = case expr of
            Nothing -> f
            (Just g) -> Lib.compose f g

  op <- optional $ do
    op <- parseOperator
    h <- parseExpression
    return $ \x -> Lib.runFunction g x `op` Lib.runFunction h x

  case op of
    Nothing -> return g
    (Just h) -> return $ Lib.Function h