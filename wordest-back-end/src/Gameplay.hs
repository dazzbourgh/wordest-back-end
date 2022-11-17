module Gameplay where

import Data.ByteString.Char8 (pack)
import Data.List.NonEmpty (toList)
import Data.Trie (insert, member)
import qualified Model as M

data TurnError = WordAlreadyUsed | WordNotInDictionary
  deriving (Show, Eq)

useWord :: M.Word -> M.Dictionary -> M.Dictionary -> Either TurnError M.Dictionary
useWord word usedWords allowedWords =
  do
    let s = pack $ toList word
    s' <- if s `member` allowedWords then Right s else Left WordNotInDictionary
    usedWords' <- if not $ s `member` usedWords then Right usedWords else Left WordAlreadyUsed
    Right (insert s' () usedWords')

takeTurn :: M.Word -> M.Game -> Either TurnError M.Game
takeTurn word (M.Game activePlayer players playersCount usedWords allowedWords stage) =
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
          M.playersCount = playersCount,
          M.usedWords = usedWords',
          M.allowedWords = allowedWords,
          M.gameStage = stage
        }
