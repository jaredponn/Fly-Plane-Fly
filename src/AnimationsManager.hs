{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleContexts #-}

module AnimationsManager where

import Control.Monad.State
import Control.Lens

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
        getPlayerAnimationSrc = headAnimation <$> use (vRenderingVars.playerAnimationHandler) 

        updatePlayerAnimation :: (MonadState Vars m, TimeManager m) => m ()
        updatePlayerAnimation = do
                playeranimationhandler <- use $ vRenderingVars.playerAnimationHandler
                -- adds the dt to the accumlated time of the animation
                dt <- getdt
                let nplayeranimationhandler = addTimeToAnimationHandler playeranimationhandler dt
                -- iterates to the next animation if enough time has elapsed
                vRenderingVars.playerAnimationHandler .= updateAnimationHandler nplayeranimationhandler

        removePlayerAnimationsUpto :: MonadState Vars m => AnimationType -> m ()
        removePlayerAnimationsUpto animationtype = do
                playeranimationhandler <- use $ vRenderingVars.playerAnimationHandler
                vRenderingVars.playerAnimationHandler .= removeAnimationsUpto animationtype playeranimationhandler

        prependToPlayerAnimation :: MonadState Vars m => [AnimationSrcRect] -> m ()
        prependToPlayerAnimation animations = do
                playeranimationhandler <- use $ vRenderingVars.playerAnimationHandler
                vRenderingVars.playerAnimationHandler .= prefixAnimation animations playeranimationhandler

