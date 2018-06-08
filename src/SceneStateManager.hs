{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE BangPatterns #-} 
module SceneStateManager where

import GameVars
import Logger

import Control.Monad.State
import Control.Lens

class Monad m => SceneStateManager m where
        setSceneState :: SceneState -> m ()
        viewSceneState :: m (SceneState)


instance SceneStateManager FlyPlaneFly where
        setSceneState :: (MonadState Vars m, Logger m) => SceneState -> m ()
        setSceneState !nstate = vSceneState .= nstate  

        viewSceneState :: (MonadState Vars m) => m (SceneState)
        viewSceneState = use vSceneState

