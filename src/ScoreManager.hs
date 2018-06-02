{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module ScoreManager where

import Control.Monad.State
import Control.Lens

import GameVars

class Monad m => ScoreManager m where
        incrementScore :: m ()
        getScore :: m (Int)
        resetScore :: m ()

        isHighScore :: Int -> m (Bool)
        -- takes the score it replaces and modifies the internal state
        modifyHighScore :: Int -> m ()
        getHighScore :: m Int

instance ScoreManager MahppyBird where
        getScore :: MonadState Vars m => m (Int)
        getScore = use $ vPlayVars.score

        incrementScore :: MonadState Vars m => m ()
        incrementScore = vPlayVars.score += 1

        resetScore :: MonadState Vars m => m ()
        resetScore = vPlayVars.score .= 0

        isHighScore :: MonadState Vars m => Int -> m (Bool)
        isHighScore score = uses highScore (<score)


        modifyHighScore :: MonadState Vars m => Int -> m ()
        modifyHighScore score = highScore .= score

        getHighScore :: MonadState Vars m => m Int
        getHighScore = use highScore


