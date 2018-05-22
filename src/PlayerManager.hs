{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module PlayerManager where

import Linear.V2
import Control.Monad.Reader
import Control.Monad.State

import Aabb
import GameVars

class Monad m => PlayerManager m where
        getPlayerPos :: m (V2 Float)
        getPlayerAttributes :: m (V2 Float, V2 Float) -- position, V2 width height
        getPlayerYVel :: m (Float)
        getPlayerXVel :: m (Float)

        setPlayerPos :: V2 Float -> m ()
        setPlayerYVel :: Float -> m ()
        translatePlayer:: V2 Float -> m ()

        jumpPlayer :: m ()

        getPlayerAabb :: m Aabb 
        resetPlayerPos :: m ()

instance PlayerManager MahppyBird where
        getPlayerPos ::(MonadState Vars m) => m (V2 Float)
        getPlayerPos = playerPos <$> gets vPlayVars

        getPlayerAttributes ::(MonadState Vars m, MonadReader Config m) => m (V2 Float, V2 Float)
        getPlayerAttributes = do
                ppos <- playerPos <$> gets vPlayVars
                psize <- gets cPlayerSize
                return (ppos, psize)

        getPlayerAabb :: PlayerManager m => m (Aabb)
        getPlayerAabb = do
                (V2 x y, V2 wlength hlength) <- getPlayerAttributes
                return $ Aabb (V2 x y) (V2 (x + wlength) (y + hlength))

        resetPlayerPos :: MonadState Vars m => m ()
        resetPlayerPos = do
                playvars <- gets vPlayVars
                modify (\v -> v { vPlayVars = playvars { playerPos = 0 }  })

        getPlayerYVel :: MonadState Vars m => m (Float)
        getPlayerYVel = vel <$> gets vPlayVars

        getPlayerXVel :: MonadState Vars m => m (Float)
        getPlayerXVel = gets cRightVel

        setPlayerPos :: MonadState Vars m => V2 Float -> m ()
        setPlayerPos npos = do
                playvars <- gets vPlayVars
                modify (\v -> v { vPlayVars = playvars { playerPos = npos }  })

        setPlayerYVel :: MonadState Vars m => Float -> m ()
        setPlayerYVel nvel = do
                playvars <- gets vPlayVars
                modify (\v -> v { vPlayVars = playvars { vel = nvel }  })

        jumpPlayer :: (MonadState Vars m, MonadReader Config m, PlayerManager m) => m ()
        jumpPlayer = gets cJumpHeight >>= setPlayerYVel

        translatePlayer :: PlayerManager m => V2 Float -> m ()
        translatePlayer transform = do
                curpos <- getPlayerPos
                setPlayerPos $ transform + curpos

