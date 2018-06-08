{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE BangPatterns #-} 
module PlayerManager where

import Linear.V2
import qualified SDL
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Foreign.C.Types

import Aabb
import GameVars

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

        isPlayerJumping :: m (Bool)
        getPlayerJumpHeight :: m (Float)

        getPlayerAabb :: m Aabb 

        getPlayerAngle :: m (CDouble)
        setPlayerAngle :: CDouble -> m ()

instance PlayerManager FlyPlaneFly where
        getPlayerPos ::(MonadState Vars m) => m (SDL.Point V2 Float)
        getPlayerPos = do
                SDL.Rectangle pos _  <- use $ vPlayVars.player.attributes
                return pos

        getPlayerAttributes ::(MonadState Vars m, MonadReader Config m) => m (SDL.Rectangle Float)
        getPlayerAttributes = use $ vPlayVars.player.attributes

        getPlayerAabb :: PlayerManager m => m (Aabb)
        getPlayerAabb = do
                curplayer <- getPlayerAttributes
                return $ rectangleToAabb curplayer


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

        translatePlayer :: PlayerManager m => V2 Float -> m ()
        translatePlayer !n = do
                SDL.P curpos <- getPlayerPos
                setPlayerPos . SDL.P $ n + curpos

        isPlayerJumping :: PlayerManager m => m (Bool)
        isPlayerJumping = (<0) <$> getPlayerYVel 

        getPlayerJumpHeight :: MonadState Vars m => m (Float)
        getPlayerJumpHeight = use $ vPlayVars.player.jumpHeight

        setIsPassingWall :: MonadState Vars m => Bool -> m ()
        setIsPassingWall !n = vPlayVars.player.isPassingWall .= n

        getIsPassingWall :: MonadState Vars m => m (Bool)
        getIsPassingWall = use $ vPlayVars.player.isPassingWall

        getPlayerAngle :: MonadState Vars m => m (CDouble)
        getPlayerAngle = use $ vPlayVars.player.GameVars.angle

        setPlayerAngle :: MonadState Vars m => CDouble -> m ()
        setPlayerAngle !n= vPlayVars.player.GameVars.angle .= n


updatePlayerAngle :: PlayerManager m => m ()
updatePlayerAngle = do
        curyvel <- getPlayerYVel
        ang <- getPlayerAngle
        if curyvel < 0 && ang >= (-10)
           then setPlayerAngle (ang - 1)
           else if ang <= 0
           then setPlayerAngle (ang + 1)
           else return ()

resetPlayerPos :: (PlayerManager m) => m ()
resetPlayerPos = setPlayerPos $ SDL.P $ V2 0 260

resetPlayerAngle :: PlayerManager m => m ()
resetPlayerAngle = setPlayerAngle 0

jumpPlayer :: (MonadReader Config m, PlayerManager m) => m ()
jumpPlayer =  setPlayerYVel =<< getPlayerJumpHeight
