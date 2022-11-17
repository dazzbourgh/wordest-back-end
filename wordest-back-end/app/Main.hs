import Application ()
-- for YesodDispatch instance

import qualified Data.Array as A
import Data.ByteString.Char8 (pack)
import Data.List.NonEmpty (fromList)
import qualified Data.Trie as T
import Error.Handler (play)
import Foundation
import qualified Model as M
import Yesod.Core

--main :: IO ()
--main = warp 3000 App

main :: IO ()
main =
  let game =
        M.Game
          { M.activePlayer = 0,
            M.players = A.listArray (0, 2) [M.Player "Leo" "0", M.Player "Hunber" "1"],
            M.playersCount = 2,
            M.usedWords = T.empty,
            M.allowedWords = T.fromList $ map (\w -> (pack w, ())) ["food", "sleep", "plane", "train"],
            M.gameStage = M.Started
          }
      go :: M.Game -> IO ()
      go (M.Game ap _ _ _ _ M.Complete) = do putStrLn $ "Winner: " ++ show ap
      go game' =
        do
          putStrLn $ "It's " ++ show (M.activePlayer game') ++ "'s turn"
          word <- readLn
          let game'' = play game' (\_ g -> g) $ fromList word
          go game''
   in go game
