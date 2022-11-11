module Gameplay where

import Data.ByteString.Char8 (pack)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Trie (insert, member)
import Model (Dictionary)

data TurnError = WordAlreadyUsed | WordNotInDictionary
  deriving (Show, Eq)

useWord :: NonEmpty Char -> Dictionary -> Dictionary -> Either TurnError Dictionary
useWord word usedWords allowedWords =
  do
    let s = pack $ toList word
    s' <- if s `member` allowedWords then Right s else Left WordNotInDictionary
    usedWords' <- if not $ s `member` usedWords then Right usedWords else Left WordAlreadyUsed
    Right (insert s' () usedWords')
