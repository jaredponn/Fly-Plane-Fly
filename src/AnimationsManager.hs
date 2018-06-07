{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module AnimationsManager where

import Control.Monad.State
import Control.Monad.Reader
import Control.Lens

import GameVars
import Animations
import TimeManager

class Monad m => AnimationsManager m where
        getPlayerAnimationSrc :: m (AnimationSrcRect)
        updatePlayerAnimation :: m ()
        removePlayerAnimationsUpto :: AnimationType -> m ()
        prependToPlayerAnimation :: [AnimationSrcRect] -> m ()
        replacePlayerAnimation :: [AnimationSrcRect] -> m ()

        getPlayerJumpAnimation :: m ([AnimationSrcRect])
        getPlayerDeathAnimation :: m ([AnimationSrcRect])
        getPlayerIdleAnimation :: m ([AnimationSrcRect])

instance AnimationsManager FlyPlaneFly where
        getPlayerAnimationSrc :: MonadState Vars m => m (AnimationSrcRect)
        getPlayerAnimationSrc = headAnimation <$> use (vRenderingVars.playerAnimationHandler) 

        updatePlayerAnimation :: (MonadState Vars m, TimeManager m) => m ()
        updatePlayerAnimation = do
                playeranimationhandler <- use $ vRenderingVars.playerAnimationHandler
                -- adds the dt to the accumlated time of the animation
                curdt <- getdt
                let nplayeranimationhandler = addTimeToAnimationHandler playeranimationhandler curdt
                -- iterates to the next animation if enough time has elapsed
                vRenderingVars.playerAnimationHandler .= updateAnimationHandler nplayeranimationhandler

        removePlayerAnimationsUpto :: MonadState Vars m => AnimationType -> m ()
        removePlayerAnimationsUpto !animationtype = do
                playeranimationhandler <- use $ vRenderingVars.playerAnimationHandler
                vRenderingVars.playerAnimationHandler .= removeAnimationsUpto animationtype playeranimationhandler

        prependToPlayerAnimation :: MonadState Vars m => [AnimationSrcRect] -> m ()
        prependToPlayerAnimation !animations = do
                playeranimationhandler <- use $ vRenderingVars.playerAnimationHandler
                vRenderingVars.playerAnimationHandler .= prefixAnimation animations playeranimationhandler

        replacePlayerAnimation :: MonadState Vars m => [AnimationSrcRect] -> m ()
        replacePlayerAnimation animations = do 
                playeranimationhandler <- use $ vRenderingVars.playerAnimationHandler
                vRenderingVars.playerAnimationHandler .= replaceAnimation animations playeranimationhandler

        getPlayerJumpAnimation :: MonadReader Config m => m ([AnimationSrcRect])
        getPlayerJumpAnimation = view (cResources.cAnimations.playerJumpAnimation)

        getPlayerDeathAnimation :: MonadReader Config m => m ([AnimationSrcRect])
        getPlayerDeathAnimation = view (cResources.cAnimations.playerDeathAnimation)

        getPlayerIdleAnimation :: MonadReader Config m => m ([AnimationSrcRect])
        getPlayerIdleAnimation = view (cResources.cAnimations.playerIdleAnimation)
