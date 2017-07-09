module Parser where

import           Control.Applicative
import qualified Control.Monad             as M (join)
import           Control.Monad.IO.Class
import qualified Data.List                 as L
import           Data.Matrix.Dense.Generic
import           Data.Maybe                (fromMaybe)
import qualified Data.Scientific           as Sci
import           Data.Vector
import           Function
import qualified Library                   as Lib
import           Stringify
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

parseDecimal :: Parser Double
parseDecimal = fmap (either fromInteger Sci.toRealFloat) $ runUnspaced $ do
  _ <- many (oneOf space')
  integerOrScientific

parseList :: Parser a -> Parser [a]
parseList p = p `sepEndBy1` some (oneOf eol)

parseVector :: Parser a -> Parser (Vector a)
parseVector p = fromList <$> p `sepEndBy1` some (oneOf space')

parseVectors :: Parser a -> Parser [Vector a]
parseVectors = parseList . parseVector

parseMatrix :: Parser a -> Parser (Matrix Vector a)
parseMatrix p = fromRows <$> parseVectors p

parseFunction :: Parser Function
parseFunction = choice
  [ parseConst
  , parseX
  , parseSin
  , parseCos
  , parseLog
  , parseExp
  , parseSqrt
  , parseAbs
  ]

parseConst :: Parser Function
parseConst = do
  x <- parseDecimal
  return $ Function (show x) (const $ Right x)

parseX :: Parser Function
parseX = runUnspaced $ do
  char 'x'
  maybeIndex <- optional natural
  let index = fromMaybe 0 maybeIndex

  return $ Function ("x" L.++ show index) $ Right . (Data.Vector.! fromIntegral index)

parseSin :: Parser Function
parseSin = string "sin" >> pure (simpleFunc "sin" $ Right . sin)

parseCos :: Parser Function
parseCos = string "cos" >> pure (simpleFunc "cos" $ Right . cos)

parseLog :: Parser Function
parseLog = string "log" >> pure (simpleFunc "log" (\x -> if x > 0 then Right (log x) else Left (Lib.ArgumentOutOfRange $ stringify x L.++ " must be positive (log)")))

parseSqrt :: Parser Function
parseSqrt = string "sqrt" >> pure (simpleFunc "sqrt" (\x -> if x >= 0 then Right (sqrt x) else Left (Lib.ArgumentOutOfRange $ stringify x L.++ " must be positive (sqrt)")))

parseExp :: Parser Function
parseExp = string "exp" >> pure (simpleFunc "exp" $ Right . exp)

parseAbs :: Parser Function
parseAbs = string "abs" >> pure (simpleFunc "abs" $ Right . abs)


operators = "+-*/^"

type Operator = Double -> Double -> Either Lib.ComputeError Double

parseOperator :: Parser (String, Operator)
parseOperator = do
  op <- oneOf operators
  return
    $ (,) (show op)
    $ case op of
        '+' -> \x y -> Right (x + y)
        '-' -> \x y -> Right (x - y)
        '*' -> \x y -> Right (x * y)
        '/' -> \x y -> if Lib.nearZero y then Left (Lib.ArgumentOutOfRange "Division by zero") else Right (x / y)
        '^' -> \x y -> Right (x ** y)

parseOpOperand :: Function -> Parser Function
parseOpOperand g = do
  op <- optional $ do
    (sop, op) <- parseOperator
    h <- parseExpression

    return
      $ Function (show g L.++ sop L.++ show h)
      $ \x -> M.join $ op <$> runFunction g x <*> runFunction h x

  case op of
    Nothing  -> return g
    (Just h) -> return h

-- operators, function compositions ...
parseExpression :: Parser Function
parseExpression = do
  subExpr <- parseSubExpression

  case subExpr of
    (Just g) -> parseOpOperand g

    Nothing -> do
      f <- parseFunction
      args <- parseSubExpression

      let g = case args of
                Nothing  -> f
                (Just g) -> compose f g

      parseOpOperand g

parseSubExpression :: Parser (Maybe Function)
parseSubExpression = optional (char '(' *> parseExpression <* char ')')
