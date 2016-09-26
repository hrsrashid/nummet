module Main where

import Test.Hspec
import Text.Trifecta
import Data.Number.CReal
import qualified Data.Vector as Vec
import qualified Data.Matrix as Mx

import Parser
import Stringify
import qualified SlaeGauss

main :: IO ()
main = hspec $ do
  describe "Parsing decimal" $ do
    let parse = toEither . parseString parseDecimal mempty

    it "parse just int" $
      parse "5" `shouldBe` Right 5

    it "parse just real" $
      parse "3.43" `shouldBe` Right 3.43

    it "parse with preceding spaces" $
      parse " \t 4" `shouldBe` Right 4

  describe "Parsing vector" $ do
    let parse s = Vec.toList <$> toEither (parseString parseVector mempty s)

    it "parse ints" $
      parse "1  2  3" `shouldBe` Right [1, 2, 3]

    it "parse floats with tabs and trailing space" $
      parse "4.9\t8.3\t9 10.5 \t" `shouldBe` Right [4.9, 8.3, 9, 10.5]

  describe "Parsing matrix" $ do
    let parse s = fmap Vec.toList <$> toEither (parseString parseVectors mempty s)
    let testMatrix = Right ([[1, 2, 3], [4, 5, 6], [7, 8, 9]])

    it "parse matrix of ints" $
      parse "1 2 3\n4 5 6\n7 8 9" `shouldBe` testMatrix

    it "parse matrix with trailing eol" $
      parse "1 2 3\n4 5 6\n7 8 9\n" `shouldBe` testMatrix

  describe "Stringification" $ do
    it "stringifies vector" $
      stringify (Vec.fromList [1, 2, 3.5 :: CReal]) `shouldBe` "1.0\t2.0\t3.5"

  describe "SlaeGauss" $ do
    let compute = Vec.toList . SlaeGauss.compute . Mx.fromLists

    it "solve simple 2x2" $
      compute [[4, 5, 14], [99, 3, 105]] `shouldBe` [1, 2]

    it "solve 3x3 with fractional" $
      compute [[1, 1, 1, 3], [1, 2, 3.5, 6.5], [0.5, 0.1, 0.2, 0.8]]
        `shouldBe` [1, 1, 1]

    it "solve with fractional result" $
      compute [[1, 1, 1.6], [1, 2, 3.1]] `shouldBe` [0.1, 1.5]

    it "solve 3x3 x2" $
      compute [[1, 2, 3, 32.6], [10, 2, 1, 12.5], [2.5, 3, 1, 12.25]]
        `shouldBe` [0.1, 0.5, 10.5]

    it "solve 3x3 x3" $
      compute [[10, 20, 0.5, 12], [9, 2, 1, 9], [10, 40, 1, 19]]
        `shouldBe` [0.5, 0.25, 4]


toEither :: Result a -> Either String a
toEither (Success a) = Right a
toEither (Failure e) = Left $ show e

