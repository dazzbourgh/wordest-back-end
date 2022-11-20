module GameplaySpec where

import qualified Data.Array as A
import Data.ByteString.Char8 (pack)
import Data.Either.Utils (fromLeft, fromRight)
import Data.List.NonEmpty (fromList)
import qualified Data.Trie as T
import Gameplay (TurnError (WordAlreadyUsed, WordNotInDictionary, LettersMismatch), takeTurn)
import qualified Model as M
import Test.Hspec

spec :: Spec
spec =
  describe "takeTurn" $ do
    it "should return error if word is not allowed" $ do
      let word = fromList "test"
      let usedWords = T.empty
      let allowedWords = T.empty
      let game =
            M.Game
              { M.activePlayer = 0,
                M.players = A.array (0, 1) [(0, M.Player "test" "id")],
                M.usedWords = usedWords,
                M.allowedWords = allowedWords,
                M.previousWord = Nothing,
                M.gameStage = M.Started
              }
      let result = fromLeft $ takeTurn word game
      result `shouldBe` WordNotInDictionary
    it "should return error if word has already been used" $ do
      let word = fromList "test"
      let usedWords = T.singleton (pack "test") ()
      let allowedWords = T.singleton (pack "test") ()
      let game =
            M.Game
              { M.activePlayer = 0,
                M.players = A.array (0, 1) [(0, M.Player "test" "id")],
                M.usedWords = usedWords,
                M.allowedWords = allowedWords,
                M.previousWord = Nothing,
                M.gameStage = M.Started
              }
      let result = fromLeft $ takeTurn word game
      result `shouldBe` WordAlreadyUsed
    it "should return error if word does not start with last letter of previous word" $ do
      let word = fromList "pumpkin"
      let usedWords = T.singleton (pack "test") ()
      let allowedWords = T.fromList [(pack "test", ()), (pack "team", ())]
      let game =
            M.Game
              { M.activePlayer = 0,
                M.players = A.array (0, 1) [(0, M.Player "test" "id")],
                M.usedWords = usedWords,
                M.allowedWords = allowedWords,
                M.previousWord = Just $ fromList "test",
                M.gameStage = M.Started
              }
      let result = fromLeft $ takeTurn word game
      result `shouldBe` LettersMismatch
    it "should return updated used words dictionary when no previous word" $ do
      let word = fromList "test"
      let usedWords = T.empty
      let allowedWords = T.singleton (pack "test") ()
      let game =
            M.Game
              { M.activePlayer = 1,
                M.players = A.array (0, 1) [(0, M.Player "test1" "id1"), (1, M.Player "test2" "id2")],
                M.usedWords = usedWords,
                M.allowedWords = allowedWords,
                M.previousWord = Nothing,
                M.gameStage = M.Started
              }
      let game' = fromRight $ takeTurn word game
      let activePlayer' = M.activePlayer game'
      let usedWords' = M.usedWords game'
      activePlayer' `shouldBe` 0
      usedWords' `shouldBe` allowedWords
    it "should return updated used words dictionary when word starts with previous word last letter" $ do
      let word = fromList "team"
      let usedWords = T.singleton (pack "test") ()
      let allowedWords = T.fromList [(pack "test", ()), (pack "team", ())]
      let game =
            M.Game
              { M.activePlayer = 0,
                M.players = A.array (0, 1) [(0, M.Player "test" "id")],
                M.usedWords = usedWords,
                M.allowedWords = allowedWords,
                M.previousWord = Just $ fromList "test",
                M.gameStage = M.Started
              }
      let game' = fromRight $ takeTurn word game
      let activePlayer' = M.activePlayer game'
      let usedWords' = M.usedWords game'
      activePlayer' `shouldBe` 1
      usedWords' `shouldBe` allowedWords
