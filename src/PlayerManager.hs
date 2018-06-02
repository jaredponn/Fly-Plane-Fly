{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module PlayerManager where

import Linear.V2
import qualified SDL
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Foreign.C.Types

import Aabb
import GameVars
import AnimationsManager

class Monad m => PlayerManager m where
        getPlayerPos :: m (SDL.Point V2 Float)
        getPlayerAttributes :: m (SDL.Rectangle Float)
        getPlayerYVel :: m (Float)
        getPlayerXVel :: m (Float)

        setPlayerPos :: SDL.Point V2 Float -> m ()
        setPlayerYVel :: Float -> m ()
        translatePlayer:: V2 Float -> m ()

        setIsPassingWall :: Bool -> m ()
        getIsPassingWall :: m (Bool)

        jumpPlayer :: m ()
        isPlayerJumping :: m (Bool)

        getPlayerAabb :: m Aabb 

        getPlayerAngle :: m (CDouble)
        setPlayerAngle :: CDouble -> m ()

instance PlayerManager MahppyBird where
        getPlayerPos ::(MonadState Vars m) => m (SDL.Point V2 Float)
        getPlayerPos = do
                SDL.Rectangle pos _  <- use $ vPlayVars.player.attributes
                return pos

        getPlayerAttributes ::(MonadState Vars m, MonadReader Config m) => m (SDL.Rectangle Float)
        getPlayerAttributes = use $ vPlayVars.player.attributes

        getPlayerAabb :: PlayerManager m => m (Aabb)
        getPlayerAabb = do
                player <- getPlayerAttributes
                return $ rectangleToAabb player


        getPlayerYVel :: MonadState Vars m => m (Float)
        getPlayerYVel = use $ vPlayVars.player.yvel

        getPlayerXVel :: MonadState Vars m => m (Float)
        getPlayerXVel = use $ vPlayVars.player.xvel

        setPlayerPos :: (MonadState Vars m, PlayerManager m) => SDL.Point V2 Float -> m ()
        setPlayerPos npos = do
                SDL.Rectangle _ lengths <- use $ vPlayVars.player.attributes
                vPlayVars.player.attributes .= SDL.Rectangle npos lengths

        setPlayerYVel :: MonadState Vars m => Float -> m ()
        setPlayerYVel nvel = vPlayVars.player.yvel .= nvel

        jumpPlayer :: (MonadState Vars m, MonadReader Config m, PlayerManager m, AnimationsManager m) => m ()
        jumpPlayer = join $ uses (vPlayVars.player.cJumpHeight) setPlayerYVel 

        translatePlayer :: PlayerManager m => V2 Float -> m ()
        translatePlayer transform = do
                SDL.P curpos <- getPlayerPos
                setPlayerPos . SDL.P $ transform + curpos

        isPlayerJumping :: PlayerManager m => m (Bool)
        isPlayerJumping = (<0) <$> getPlayerYVel 

        setIsPassingWall :: MonadState Vars m => Bool -> m ()
        setIsPassingWall passing = vPlayVars.player.isPassingWall .= passing

        getIsPassingWall :: MonadState Vars m => m (Bool)
        getIsPassingWall = use $ vPlayVars.player.isPassingWall

        getPlayerAngle :: MonadState Vars m => m (CDouble)
        getPlayerAngle = use $ vPlayVars.player.GameVars.angle

        setPlayerAngle :: MonadState Vars m => CDouble -> m ()
        setPlayerAngle nangle = vPlayVars.player.GameVars.angle .= nangle


