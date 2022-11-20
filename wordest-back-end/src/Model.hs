module Model where

import Data.Array.IArray (Array (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Trie (Trie (..))
import Prelude hiding (Word)

data Player = Player
  { playerName :: String,
    playerId :: String
  }

data GameStage = Started | Complete
  deriving (Show)

type Dictionary = Trie ()

type PlayerNumber = Int

type Word = NonEmpty Char

data Game = Game
  { activePlayer :: PlayerNumber,
    players :: Array PlayerNumber Player,
    usedWords :: Dictionary,
    allowedWords :: Dictionary,
    previousWord :: Maybe Word,
    gameStage :: GameStage
  }

data Message = Message
  { messagePlayerId :: String,
    word :: Word
  }
