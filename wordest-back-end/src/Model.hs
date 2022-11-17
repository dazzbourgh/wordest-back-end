module Model where

import Data.Array.IArray (Array (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Trie (Trie (..))

data Player = Player
  { playerName :: String,
    playerId :: String
  }

data GameStage = Started | Complete

type Dictionary = Trie ()

type PlayerNumber = Int

type Word = NonEmpty Char

data Game = Game
  { activePlayer :: PlayerNumber,
    players :: Array PlayerNumber Player,
    playersCount :: Int,
    usedWords :: Dictionary,
    allowedWords :: Dictionary,
    gameStage :: GameStage
  }
