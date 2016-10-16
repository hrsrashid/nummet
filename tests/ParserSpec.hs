module ParserSpec where

import qualified Data.Vector   as Vec
import           Parser
import           Test.Hspec
import           Text.Trifecta


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

  describe "Parsing vector" $ do
    let parse s = Vec.toList <$> toEither (parseString parseVector mempty s)

    it "of ints" $
      parse "1  2  3" `shouldBe` Right [1, 2, 3]

    it "of floats with tabs and trailing space" $
      parse "4.9\t8.3\t9 10.5 \t" `shouldBe` Right [4.9, 8.3, 9, 10.5]

  describe "Parsing matrix" $ do
    let parse s = fmap Vec.toList <$> toEither (parseString parseVectors mempty s)
    let testMatrix = Right [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

    it "of ints" $
      parse "1 2 3\n4 5 6\n7 8 9" `shouldBe` testMatrix

    it "with trailing eol" $
      parse "1 2 3\n4 5 6\n7 8 9\n" `shouldBe` testMatrix
