module ParserSpec where

import qualified Data.Vector   as Vec
import           Parser
import           Test.Hspec
import           Text.Trifecta
import qualified Library       as Lib


toEither :: Result a -> Either String a
toEither (Success a) = Right a
toEither (Failure e) = Left $ show e


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
      parse parseFunction "345" `shouldBe` Right (Lib.Function (const 345))

    it "x" $
      parse parseFunction "x" `shouldBe` Right (Lib.Function id)

    it "sin" $
      parse parseFunction "sin" `shouldBe` Right (Lib.Function sin)

    it "log" $
      parse parseFunction "log" `shouldBe` Right (Lib.Function log)

    it "exp" $
      parse parseFunction "exp" `shouldBe` Right (Lib.Function exp)

    it "composition sin(x)" $
      parse parseExpression "sin(x)" `shouldBe` Right (Lib.Function sin)

    it "composition sin(log(x))" $
      parse parseExpression "sin(log(x))" `shouldBe` Right (Lib.compose (Lib.Function sin) (Lib.Function log))

    it "of sum of funcs" $
      parse parseExpression "sin(x)+log(x)" `shouldBe` Right (Lib.Function (\x -> sin x + log x))

    it "of fraction of funcs" $
      parse parseExpression "sin(x)/exp(x)" `shouldBe` Right (Lib.Function (\x -> sin x / exp x))

  
  describe "Parsing vector" $ do
    let parse p s = Vec.toList <$> toEither (parseString (parseVector p) mempty s)

    it "of ints" $
      parse parseDecimal "1  2  3" `shouldBe` Right [1, 2, 3]

    it "of floats with tabs and trailing space" $
      parse parseDecimal "4.9\t8.3\t9 10.5 \t" `shouldBe` Right [4.9, 8.3, 9, 10.5]

    it "of funcs" $
      parse parseExpression "1 2 sin(x) exp(x)+sin(log(x))" `shouldBe` Right
        [ Lib.Function (const 1)
        , Lib.Function (const 2)
        , Lib.Function sin
        , Lib.Function (\x -> exp x + sin (log x))
        ]

  describe "Parsing matrix" $ do
    let parse p s = fmap Vec.toList <$> toEither (parseString (parseVectors p) mempty s)
    let testMatrix = Right [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

    it "of ints" $
      parse parseDecimal "1 2 3\n4 5 6\n7 8 9" `shouldBe` testMatrix

    it "with trailing eol" $
      parse parseDecimal "1 2 3\n4 5 6\n7 8 9\n" `shouldBe` testMatrix

    it "of funcs" $
      parse parseExpression "1 4 sin(x)\n5 6 exp(sin(x))\n9 10 x/9" `shouldBe` Right
        [ [Lib.Function (const 1), Lib.Function (const 4), Lib.Function sin]
        , [Lib.Function (const 5), Lib.Function (const 6), Lib.Function (exp . sin)]
        , [Lib.Function (const 9), Lib.Function (const 10), Lib.Function (/9)]
        ]