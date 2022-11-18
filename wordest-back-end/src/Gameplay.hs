module Gameplay where

import Data.ByteString.Char8 (pack)
import Data.List.NonEmpty (toList)
import Data.Trie (insert, member)
import qualified Model as M

data TurnError = WordAlreadyUsed | WordNotInDictionary
  deriving (Show, Eq)

takeTurn :: M.Word -> M.Game -> Either TurnError M.Game
takeTurn word (M.Game activePlayer players usedWords allowedWords stage) =
  do
    let s = pack $ toList word
    s' <-
      if s `member` allowedWords
        then Right s
        else Left WordNotInDictionary
    usedWords' <-
      if not $ s `member` usedWords
        then Right (insert s' () usedWords)
        else Left WordAlreadyUsed
    Right
      M.Game
        { M.activePlayer = (activePlayer + 1) `mod` length players,
          M.players = players,
          M.usedWords = usedWords',
          M.allowedWords = allowedWords,
          M.gameStage = stage
        }
