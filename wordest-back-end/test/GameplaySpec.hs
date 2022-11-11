module GameplaySpec where

import Data.List.NonEmpty (fromList)
import Data.Trie (empty, singleton)
import Gameplay (TurnError (WordNotInDictionary, WordAlreadyUsed), useWord)
import Test.Hspec
import Data.ByteString.Char8 (pack)

spec :: Spec
spec =
  describe "useWord" $ do
    it "should return error if word is not allowed" $ do
      let word = fromList "test"
      let usedWords = empty
      let allowedWords = empty
      let result = useWord word usedWords allowedWords
      result `shouldBe` Left WordNotInDictionary
    it "should return error if word has already been used" $ do
          let word = fromList "test"
          let usedWords = singleton (pack "test") ()
          let allowedWords = singleton (pack "test") ()
          let result = useWord word usedWords allowedWords
          result `shouldBe` Left WordAlreadyUsed
    it "should return updated used words dictionary" $ do
              let word = fromList "test"
              let usedWords = empty
              let allowedWords = singleton (pack "test") ()
              let result = useWord word usedWords allowedWords
              result `shouldBe` Right allowedWords