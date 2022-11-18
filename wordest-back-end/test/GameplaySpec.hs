module GameplaySpec where

import qualified Data.Array as A
import Data.ByteString.Char8 (pack)
import Data.Either.Utils (fromLeft, fromRight)
import Data.List.NonEmpty (fromList)
import Data.Trie (empty, singleton)
import Gameplay (TurnError (WordAlreadyUsed, WordNotInDictionary), takeTurn)
import qualified Model as M
import Test.Hspec

spec :: Spec
spec =
  describe "takeTurn" $ do
    it "should return error if word is not allowed" $ do
      let word = fromList "test"
      let usedWords = empty
      let allowedWords = empty
      let game =
            M.Game
              { M.activePlayer = 0,
                M.players = A.array (0, 1) [(0, M.Player "test" "id")],
                M.usedWords = usedWords,
                M.allowedWords = allowedWords,
                M.gameStage = M.Started
              }
      let result = fromLeft $ takeTurn word game
      result `shouldBe` WordNotInDictionary
    it "should return error if word has already been used" $ do
      let word = fromList "test"
      let usedWords = singleton (pack "test") ()
      let allowedWords = singleton (pack "test") ()
      let game =
            M.Game
              { M.activePlayer = 0,
                M.players = A.array (0, 1) [(0, M.Player "test" "id")],
                M.usedWords = usedWords,
                M.allowedWords = allowedWords,
                M.gameStage = M.Started
              }
      let result = fromLeft $ takeTurn word game
      result `shouldBe` WordAlreadyUsed
    it "should return updated used words dictionary" $ do
      let word = fromList "test"
      let usedWords = empty
      let allowedWords = singleton (pack "test") ()
      let game =
            M.Game
              { M.activePlayer = 1,
                M.players = A.array (0, 1) [(0, M.Player "test1" "id1"), (1, M.Player "test2" "id2")],
                M.usedWords = usedWords,
                M.allowedWords = allowedWords,
                M.gameStage = M.Started
              }
      let game' = fromRight $ takeTurn word game
      let activePlayer' = M.activePlayer game'
      let usedWords' = M.usedWords game'
      activePlayer' `shouldBe` 0
      usedWords' `shouldBe` allowedWords
