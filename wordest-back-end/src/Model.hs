module Model where

import Data.Array.IArray (Array (..))
import Data.Trie (Trie (..))

data Player = Player
  { playerName :: String,
    playerId :: String
  }

data GameStage = Started | Complete

type Dictionary = Trie ()

data Game = Game
  { activePlayers :: Int,
    players :: Array Int Player,
    usedWords :: Dictionary,
    allowedWords :: Dictionary,
    gameStage :: GameStage
  }
