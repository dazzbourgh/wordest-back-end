module Error.Handler where

import Gameplay (TurnError, takeTurn)
import qualified Model as M

type ErrorHandler = TurnError -> M.Game -> M.Game

play :: M.Game -> ErrorHandler -> M.Word -> M.Game
play game handleError word =
  let eitherGame = takeTurn word game
      game' = case eitherGame of
        Right g -> g
        Left turnError -> handleError turnError game
   in game'
