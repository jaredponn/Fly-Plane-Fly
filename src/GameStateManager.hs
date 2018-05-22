{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleContexts #-} 
module GameStateManager where

import GameVars

import Control.Monad.State
import Data.Stack

class Monad m => GameStateManager m where
        pushGameState :: GameState -> m ()
        popGameState :: m (GameState)
        popGameState_ :: m ()
        peekGameState :: m (GameState)


instance GameStateManager MahppyBird where
        pushGameState :: MonadState Vars m => GameState -> m ()
        pushGameState nstate = do
                gamestack <- gets vGameStateStack
                let nstack = stackPush gamestack nstate
                modify (\v -> v { vGameStateStack = nstack } )

        popGameState :: MonadState Vars m => m (GameState)
        popGameState = do
                gamestack <- gets vGameStateStack
                -- ADD ERROR HANDLING
                let Just (nstack, val) = stackPop gamestack
                modify (\v -> v { vGameStateStack = nstack } )
                return val

        popGameState_ :: MonadState Vars m => m ()
        popGameState_ = do
                gamestack <- gets vGameStateStack
                -- ADD ERROR HANDLING
                let Just (nstack, val) = stackPop gamestack
                modify (\v -> v { vGameStateStack = nstack } )

        peekGameState :: MonadState Vars m => m (GameState)
        peekGameState = do
                gamestack <- gets vGameStateStack
                let Just (nstack, val) = stackPop gamestack
                return val
