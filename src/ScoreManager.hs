{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module ScoreManager where

import Control.Monad.State

import GameVars

class Monad m => ScoreManager m where
        incrementScore :: m ()
        getScore :: m (Int)
        resetScore :: m ()
        setIsPassingWall :: Bool -> m ()
        getIsPassingWall :: m (Bool)

instance ScoreManager MahppyBird where
        getScore :: MonadState Vars m => m (Int)
        getScore = gets score

        incrementScore :: MonadState Vars m => m ()
        incrementScore = do
                currscore <- gets score
                modify (\v -> v { score = currscore + 1} )

        resetScore :: MonadState Vars m => m ()
        resetScore = modify (\v -> v { score = 0} )

        setIsPassingWall :: MonadState Vars m => Bool -> m ()
        setIsPassingWall bool = do
                playvars <- gets vPlayVars
                modify (\v -> v {vPlayVars = playvars { isPassingWall = bool }})

        getIsPassingWall :: MonadState Vars m => m (Bool)
        getIsPassingWall = do
                playvars <- gets vPlayVars
                return $ isPassingWall playvars


