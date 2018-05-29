{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleContexts #-} 
module GameStateManager where

import GameVars

import Control.Monad.State
import Data.Stack
import Control.Lens

class Monad m => GameStateManager m where
        pushGameState :: GameState -> m ()
        popGameState :: m (GameState)
        popGameState_ :: m ()
        peekGameState :: m (GameState)


instance GameStateManager MahppyBird where
        pushGameState :: MonadState Vars m => GameState -> m ()
        pushGameState nstate = do
                {- gamestack <- gets vGameStateStack -}
                {- let nstack = stackPush gamestack nstate -}
                {- modify (\v -> v { vGameStateStack = nstack } ) -}
                vGameStateStack %= flip stackPush nstate

        popGameState :: MonadState Vars m => m (GameState)
        popGameState = do
                {- gamestack <- gets vGameStateStack -}
                {- -- ADD ERROR HANDLING -}
                {- let Just (nstack, val) = stackPop gamestack -}
                {- modify (\v -> v { vGameStateStack = nstack } ) -}
                Just (nstack, val) <- stackPop <$> use vGameStateStack
                vGameStateStack .= nstack
                return val

        popGameState_ :: MonadState Vars m => m ()
        popGameState_ = do
                Just (nstack, val) <- stackPop <$> use vGameStateStack
                vGameStateStack .= nstack

        peekGameState :: MonadState Vars m => m (GameState)
        peekGameState = do
                Just (nstack, val) <- stackPop <$> use vGameStateStack
                return val
