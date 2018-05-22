module GameStateManager where

import GameVars

class Monad m => GameStateManager m where
        pushGameState :: GameState -> m ()
        popGameState :: m (GameState)
        popGameState_ :: m ()
        peekGameState :: m (GameState)

