{-# LANGUAGE InstanceSigs #-} 
{- {-# LANGUAGE FlexibleInstances #-}  -}
{-# LANGUAGE FlexibleContexts #-}

module AnimationsManager where

import Control.Monad.State

import GameVars
import Animations
import TimeManager

class Monad m => AnimationsManager m where
        getPlayerAnimationSrc :: m (AnimationSrcRect)
        updatePlayerAnimation :: m ()
        removePlayerAnimationsUpto :: AnimationType -> m ()
        prependToPlayerAnimation :: [AnimationSrcRect] -> m ()


-- TODO make some crappy graphics for testing player animation
-- Implement it so that it works.

instance AnimationsManager MahppyBird where
        getPlayerAnimationSrc :: MonadState Vars m => m (AnimationSrcRect)
        getPlayerAnimationSrc =  gets animationVars >>= return . headAnimation . playerAnimationHandler

        updatePlayerAnimation :: (MonadState Vars m, TimeManager m) => m ()
        updatePlayerAnimation = do
                animationvars <- gets animationVars 
                dt <- getdt
                let playeranimationhandler = addTimeToAnimationHandler (playerAnimationHandler animationvars) dt
                modify (\v -> v { animationVars = animationvars {playerAnimationHandler = updateAnimationHandler playeranimationhandler} } )

        removePlayerAnimationsUpto :: MonadState Vars m => AnimationType -> m ()
        removePlayerAnimationsUpto animationtype = do
                animationvars <- gets animationVars 
                let playeranimationhandler = playerAnimationHandler animationvars
                modify (\v -> v { animationVars = animationvars {playerAnimationHandler = removeAnimationsUpto animationtype playeranimationhandler } } )

        prependToPlayerAnimation :: MonadState Vars m => [AnimationSrcRect] -> m ()
        prependToPlayerAnimation animations = do
                animationvars <- gets animationVars 
                let playeranimationhandler = playerAnimationHandler animationvars
                    modify (\v -> v { animationVars = animationvars {playerAnimationHandler = prefixAnimation animations animationtype playeranimationhandler } } )

