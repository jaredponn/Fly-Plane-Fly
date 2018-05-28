{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module PlayerManager where

import Linear.V2
import SDL
import Control.Monad.Reader
import Control.Monad.State

import Aabb
import GameVars
import AnimationsManager

class Monad m => PlayerManager m where
        getPlayerPos :: m (Point V2 Float)
        getPlayer :: m (Rectangle Float)
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
                Rectangle pos _  <- player <$> gets vPlayVars
                return pos

        getPlayer ::(MonadState Vars m, MonadReader Config m) => m (Rectangle Float)
        getPlayer = do
                player <$> gets vPlayVars

        getPlayerAabb :: PlayerManager m => m (Aabb)
        getPlayerAabb = do
                player <- getPlayer
                return $ rectangleToAabb player

        resetPlayerPos :: (MonadState Vars m, PlayerManager m) => m ()
        resetPlayerPos = do
                playvars <- gets vPlayVars
                Rectangle _ lengths <- getPlayer
                modify (\v -> v { vPlayVars = playvars { player = Rectangle (P (V2 0 0)) lengths }})

        getPlayerYVel :: MonadState Vars m => m (Float)
        getPlayerYVel = vel <$> gets vPlayVars

        getPlayerXVel :: MonadState Vars m => m (Float)
        getPlayerXVel = gets cRightVel

        setPlayerPos :: (MonadState Vars m, PlayerManager m) => Point V2 Float -> m ()
        setPlayerPos npos = do
                playvars <- gets vPlayVars
                Rectangle _ lengths <- getPlayer
                modify (\v -> v { vPlayVars = playvars { player = Rectangle npos lengths }  })

        setPlayerYVel :: MonadState Vars m => Float -> m ()
        setPlayerYVel nvel = do
                playvars <- gets vPlayVars
                modify (\v -> v { vPlayVars = playvars { vel = nvel }  })

        jumpPlayer :: (MonadState Vars m, MonadReader Config m, PlayerManager m, AnimationsManager m) => m ()
        jumpPlayer = do
                jumpheight <- gets cJumpHeight 
                setPlayerYVel jumpheight

        translatePlayer :: PlayerManager m => V2 Float -> m ()
        translatePlayer transform = do
                P curpos <- getPlayerPos
                setPlayerPos . P $ transform + curpos

        isPlayerJumping :: PlayerManager m => m (Bool)
        isPlayerJumping = (<0) <$> getPlayerYVel 

