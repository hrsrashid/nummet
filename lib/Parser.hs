module Parser where

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Data.List                 as L
import           Data.Matrix.Dense.Generic
import qualified Data.Scientific           as Sci
import           Data.Vector
import           Text.Trifecta
import qualified Library                   as Lib
import           Stringify

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
  , parseCos
  , parseLog
  , parseExp
  , parseSqrt
  , parseAbs
  ]

parseConst :: Parser Lib.Function
parseConst = do
  x <- parseDecimal
  return $ Lib.Function (show x) (const $ Right x)

parseX :: Parser Lib.Function
parseX = runUnspaced $ do
  char 'x'
  index <- natural
  return $ Lib.Function ("x" L.++ show index) $ Right . (Data.Vector.! fromIntegral index)

parseSin :: Parser Lib.Function
parseSin = string "sin" >> pure (Lib.simpleFunc "sin" $ Right . sin)

parseCos :: Parser Lib.Function
parseCos = string "cos" >> pure (Lib.simpleFunc "cos" $ Right . cos)

parseLog :: Parser Lib.Function
parseLog = string "log" >> pure (Lib.simpleFunc "log" (\x -> if x > 0 then Right (log x) else Left (Lib.ArgumentOutOfRange $ stringify x L.++ " must be positive (log)")))

parseSqrt :: Parser Lib.Function
parseSqrt = string "sqrt" >> pure (Lib.simpleFunc "sqrt" (\x -> if x >= 0 then Right (sqrt x) else Left (Lib.ArgumentOutOfRange $ stringify x L.++ " must be positive (sqrt)")))

parseExp :: Parser Lib.Function
parseExp = string "exp" >> pure (Lib.simpleFunc "exp" $ Right . exp)

parseAbs :: Parser Lib.Function
parseAbs = string "abs" >> pure (Lib.simpleFunc "abs" $ Right . abs)


operators = "+-*/^"

type Operator = Double -> Double -> Double

parseOperator :: Parser (String, Operator)
parseOperator = do
  op <- oneOf operators
  return
    $ (,) (show op)
    $ case op of
        '+' -> (+)
        '-' -> (-)
        '*' -> (*)
        '/' -> (/)
        '^' -> (**)

parseOpOperand :: Lib.Function -> Parser Lib.Function
parseOpOperand g = do
  op <- optional $ do
    (sop, op) <- parseOperator
    h <- parseExpression
    return $ Lib.Function (show g L.++ sop L.++ show h) $ \x -> op <$> Lib.runFunction g x <*> Lib.runFunction h x

  case op of
    Nothing -> return g
    (Just h) -> return h

-- operators, function compositions ...
parseExpression :: Parser Lib.Function
parseExpression = do
  subExpr <- parseSubExpression 

  case subExpr of
    (Just g) -> parseOpOperand g

    Nothing -> do
      f <- parseFunction
      args <- parseSubExpression

      let g = case args of
                Nothing -> f
                (Just g) -> Lib.compose f g

      parseOpOperand g

parseSubExpression :: Parser (Maybe Lib.Function)
parseSubExpression = optional (char '(' *> parseExpression <* char ')')
