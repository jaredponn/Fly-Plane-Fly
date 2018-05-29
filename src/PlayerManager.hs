{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module PlayerManager where

import Linear.V2
import SDL
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Aabb
import GameVars
import AnimationsManager

class Monad m => PlayerManager m where
        getPlayerPos :: m (Point V2 Float)
        getPlayerAttributes :: m (Rectangle Float)
        getPlayerYVel :: m (Float)
        getPlayerXVel :: m (Float)

        setPlayerPos :: Point V2 Float -> m ()
        setPlayerYVel :: Float -> m ()
        translatePlayer:: V2 Float -> m ()

        jumpPlayer :: m ()
        isPlayerJumping :: m (Bool)

        getPlayerAabb :: m Aabb 
        resetPlayerPos :: m ()

instance PlayerManager MahppyBird where
        getPlayerPos ::(MonadState Vars m) => m (Point V2 Float)
        getPlayerPos = do
                Rectangle pos _  <- use $ vPlayVars.player.attributes
                return pos

        getPlayerAttributes ::(MonadState Vars m, MonadReader Config m) => m (Rectangle Float)
        getPlayerAttributes = use $ vPlayVars.player.attributes

        getPlayerAabb :: PlayerManager m => m (Aabb)
        getPlayerAabb = do
                player <- getPlayerAttributes
                return $ rectangleToAabb player

        resetPlayerPos :: (PlayerManager m) => m ()
        resetPlayerPos = setPlayerPos $ P $ V2 0 0

        getPlayerYVel :: MonadState Vars m => m (Float)
        getPlayerYVel = use $ vPlayVars.player.yvel

        getPlayerXVel :: MonadState Vars m => m (Float)
        getPlayerXVel = use $ vPlayVars.player.xvel

        setPlayerPos :: (MonadState Vars m, PlayerManager m) => Point V2 Float -> m ()
        setPlayerPos npos = do
                Rectangle _ lengths <- use $ vPlayVars.player.attributes
                vPlayVars.player.attributes .= Rectangle npos lengths

        setPlayerYVel :: MonadState Vars m => Float -> m ()
        setPlayerYVel nvel = vPlayVars.player.yvel .= nvel

        jumpPlayer :: (MonadState Vars m, MonadReader Config m, PlayerManager m, AnimationsManager m) => m ()
        jumpPlayer = do
                jumpheight <- use $ vPlayVars.player.cJumpHeight
                setPlayerYVel jumpheight

        translatePlayer :: PlayerManager m => V2 Float -> m ()
        translatePlayer transform = do
                P curpos <- getPlayerPos
                setPlayerPos . P $ transform + curpos

        isPlayerJumping :: PlayerManager m => m (Bool)
        isPlayerJumping = (<0) <$> getPlayerYVel 

