module GameState (update)
  where

import Interpretor (GameState(..),
                    Command(..))

-- TODO implement!!!
update :: GameState -> Command -> GameState
update state NothingCommand            = state
update state (Command x' y' building') = state
