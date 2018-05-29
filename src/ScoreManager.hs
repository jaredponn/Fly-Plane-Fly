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
        setIsPassingWall :: Bool -> m ()
        getIsPassingWall :: m (Bool)

instance ScoreManager MahppyBird where
        getScore :: MonadState Vars m => m (Int)
        getScore = use $ vPlayVars.score

        incrementScore :: MonadState Vars m => m ()
        incrementScore = vPlayVars.score += 1

        resetScore :: MonadState Vars m => m ()
        resetScore = vPlayVars.score .= 0

        setIsPassingWall :: MonadState Vars m => Bool -> m ()
        setIsPassingWall passing = vPlayVars.player.isPassingWall .= passing

        getIsPassingWall :: MonadState Vars m => m (Bool)
        getIsPassingWall = do
                use $ vPlayVars.player.isPassingWall


