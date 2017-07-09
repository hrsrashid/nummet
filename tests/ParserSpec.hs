module ParserSpec where

import qualified Data.Vector   as Vec
import           Function
import qualified Library       as Lib
import           Parser
import           Test.Hspec
import           Text.Trifecta


toEither :: Result a -> Either String a
toEither (Success a) = Right a
toEither (Failure e) = Left $ show e

fSin = simpleFunc "sin" $ Right . sin
fLog = simpleFunc "log" $ Right . log
fExp = simpleFunc "exp" $ Right . exp


suite :: SpecWith ()
suite = do
  describe "Ignore junk" $ do
    let parse p = toEither . parseString (parseInput p) mempty
    let value = some letter
    let twoValues = do
          v1 <- value
          skipJunk
          v2 <- value
          return (v1, v2)

    it "none" $
      parse value "value" `shouldBe` Right "value"

    it "before" $
      parse value "# junk\n\n\n# junk2\n\nvalue" `shouldBe` Right "value"

    it "after" $
      parse value "value\n\n#junk\n\n#junk2\n\n" `shouldBe` Right "value"

    it "around" $
      parse value "# junk\n\n\n# junk2\n\nvalue\n\n#junk\n\n#junk2\n\n"
        `shouldBe` Right "value"

    it "between" $
      parse twoValues "value\n\n#junk\n\n#junk2\n\nvalued"
        `shouldBe` Right ("value", "valued")

  describe "Parsing decimal" $ do
    let parse = toEither . parseString parseDecimal mempty

    it "just int" $
      parse "5" `shouldBe` Right 5

    it "just real" $
      parse "3.43" `shouldBe` Right 3.43

    it "with preceding spaces" $
      parse " \t 4" `shouldBe` Right 4

  describe "Parse function" $ do
    let parse p s = toEither (parseString p mempty s)

    it "constant" $
      parse parseFunction "345" `shouldBe` Right (Function "345" (const $ Right 345))

    it "x" $
      parse parseFunction "x" `shouldBe` Right (Function "x0" $ Right . (Vec.! 0))

    it "x1" $
      parse parseFunction "x1" `shouldBe` Right (Function "x1" $ Right . (Vec.! 1))

    it "x3" $
      parse parseFunction "x3" `shouldBe` Right (Function "x3" $ Right . (Vec.! 3))

    it "sin" $
      parse parseFunction "sin" `shouldBe` Right fSin

    it "log" $
      parse parseFunction "log" `shouldBe` Right fLog

    it "exp" $
      parse parseFunction "exp" `shouldBe` Right fExp

    it "power" $
      parse parseExpression "cos(x0)^x1" `shouldBe` Right (Function "cos(x0)^x1" (\v -> Right $ cos (v Vec.! 0) ** (v Vec.! 1)))

    it "composition sin(x0)" $
      parse parseExpression "sin(x0)" `shouldBe` Right fSin

    it "composition sin(log(x0))" $
      parse parseExpression "sin(log(x0))" `shouldBe` Right (compose fSin fLog)

    it "of sum of funcs" $
      parse parseExpression "sin(x0)+log(x0)" `shouldBe` Right (simpleFunc "sin(x0)+log(x0)" (\x -> Right $ sin x + log x))

    it "of fraction of funcs" $
      parse parseExpression "sin(x0)/exp(x0)" `shouldBe` Right (simpleFunc "sin(x0)/exp(x0)" (\x -> Right $ sin x / exp x))

    it "with different vars" $
      parse parseExpression "x0*x1*x2" `shouldBe` Right (Function "x0*x1*x2" (\v -> Right $ v Vec.! 0 * v Vec.! 1 * v Vec.! 2))

    it "with multiple ( and )" $ do
      let expr = "(x0-(x1*x2))+sin((x0*x1)-(x0-x1))"
      parse parseExpression expr `shouldBe` Right (Function expr (\v -> let
          x0 = v Vec.! 0
          x1 = v Vec.! 1
          x2 = v Vec.! 2
        in Right $ (x0 - (x1*x2)) + sin ((x0*x1) - (x0 - x1)) ))

  describe "Parsing list" $ do
    let parse p s = toEither (parseString (parseList p) mempty s)

    it "of ints" $
      parse parseDecimal "1\n2\r3\n\r4\n" `shouldBe` Right [1, 2, 3, 4]

  describe "Parsing vector" $ do
    let parse p s = Vec.toList <$> toEither (parseString (parseVector p) mempty s)

    it "of ints" $
      parse parseDecimal "1  2  3" `shouldBe` Right [1, 2, 3]

    it "of floats with tabs and trailing space" $
      parse parseDecimal "4.9\t8.3\t9 10.5 \t" `shouldBe` Right [4.9, 8.3, 9, 10.5]

    it "of funcs" $
      parse parseExpression "1 2 sin(x0) exp(x0)+sin(log(x0))" `shouldBe` Right
        [ Function "1" (const $ Right 1)
        , Function "2" (const $ Right 2)
        , fSin
        , simpleFunc "exp(x0)+sin(log(x0))" (\x -> Right $ exp x + sin (log x))
        ]

  describe "Parsing matrix" $ do
    let parse p s = fmap Vec.toList <$> toEither (parseString (parseVectors p) mempty s)
    let testMatrix = Right [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

    it "of ints" $
      parse parseDecimal "1 2 3\n4 5 6\n7 8 9" `shouldBe` testMatrix

    it "with trailing eol" $
      parse parseDecimal "1 2 3\n4 5 6\n7 8 9\n" `shouldBe` testMatrix

    it "of funcs" $
      parse parseExpression "1 4 sin(x0)\n5 6 exp(sin(x0))\n9 10 x0/9" `shouldBe` Right
        [ [Function "1" (const $ Right 1), Function "4" (const $ Right 4), fSin]
        , [Function "5" (const $ Right 5), Function "6" (const $ Right 6), simpleFunc "exp(sin(x0))" (Right . exp . sin)]
        , [Function "9" (const $ Right 9), Function "10" (const $ Right 10), simpleFunc "x0/9" $ Right . (/9)]
        ]

    it "of funcs2" $
      parse parseExpression "1 2 4 sin(x0)+log(x1)\n0 3 5 x2*x1\n2 1 9 0" `shouldBe` Right
        [ [Function "1" (const $ Right 1), Function "2" (const $ Right 2), Function "4" (const $ Right 4), Function "sin(x0)+log(x1)" (\v -> Right $ sin (v Vec.! 0) + log (v Vec.! 1))]
        , [Function "0" (const $ Right 0), Function "3" (const $ Right 3), Function "5" (const $ Right 5), Function "x2*x1" (\v -> Right $ (v Vec.! 2) * (v Vec.! 1))]
        , [Function "2" (const $ Right 2), Function "1" (const $ Right 1), Function "9" (const $ Right 9), Function "0" (const $ Right 0)]
        ]
