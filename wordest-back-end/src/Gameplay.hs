module Gameplay where

import Data.ByteString.Char8 (pack)
import qualified Data.List.NonEmpty as NE
import Data.Trie (insert, member)
import qualified Model as M

data TurnError = WordAlreadyUsed | WordNotInDictionary | LettersMismatch
  deriving (Show, Eq)

checkLettersMatch :: M.Word -> M.Word -> Bool
checkLettersMatch (curC NE.:| _) prevWord = curC == NE.head (NE.reverse prevWord)

takeTurn :: M.Word -> M.Game -> Either TurnError M.Game
takeTurn word (M.Game activePlayer players usedWords allowedWords prevWord stage) =
  do
    let s = pack $ NE.toList word
    _ <- 
      case prevWord of 
        Just prevWord' -> if checkLettersMatch word prevWord' then Right () else Left LettersMismatch
        Nothing -> Right ()
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
          M.previousWord = Just word,
          M.gameStage = stage
        }
