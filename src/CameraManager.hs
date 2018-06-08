{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE BangPatterns #-} 
module CameraManager where

import Linear.V2
import SDL
import Foreign.C.Types
import Control.Monad.State
import Control.Lens

import PlayerManager 
import GameVars
import Util

class Monad m => CameraManager m where
        getCameraOffset :: m (V2 Float)

        setCameraPos :: Point V2 Float -> m ()
        moveCameraBy :: V2 CInt -> m ()

        getCameraPos :: m (Point V2 CInt)

instance CameraManager FlyPlaneFly where
        moveCameraBy :: (MonadState Vars m) => V2 CInt -> m ()
        moveCameraBy !n = vRenderingVars.cameraPos %= (+ (P n))

        setCameraPos :: (MonadState Vars m) => Point V2 Float -> m ()
        setCameraPos (P npos) = vRenderingVars.cameraPos .= (P (roundV2 npos))

        getCameraPos :: (MonadState Vars m ) => m (Point V2 CInt)
        getCameraPos = use $ vRenderingVars.cameraPos

        getCameraOffset :: MonadState Vars m => m (V2 Float)
        getCameraOffset = use $ vRenderingVars.cameraOffset

-- updateCameraPos updates the camera postion by the "cameraOffset" and the current position of the player
updateCameraPos :: (PlayerManager m, CameraManager m) => m ()
updateCameraPos = do
        SDL.P (V2 x _) <- getPlayerPos
        camoffset <- getCameraOffset
        setCameraPos . SDL.P $ (V2 x 0) + camoffset
