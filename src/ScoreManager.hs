{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module ScoreManager where

import Control.Monad.State

import GameVars

class (Monad m) => ScoreManager m where
        incrementScore :: m ()
        getScore :: m (Int)
        resetScore :: m ()

instance ScoreManager MahppyBird where
        getScore :: MonadState Vars m => m (Int)
        getScore = gets score

        incrementScore :: MonadState Vars m => m ()
        incrementScore = do
                currscore <- gets score
                modify (\v -> v { score = currscore + 1} )

        resetScore :: MonadState Vars m => m ()
        resetScore = modify (\v -> v { score = 0} )
