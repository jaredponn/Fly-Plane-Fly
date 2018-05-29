{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module CameraManager where

import Linear.V2
import SDL
import Foreign.C.Types
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import GameVars
import Util

class Monad m => CameraManager m where
        getCameraOffset :: m (V2 Float)

        setCameraPos :: Point V2 Float -> m ()
        moveCameraBy :: V2 CInt -> m ()

        getCameraPos :: m (Point V2 CInt)

instance CameraManager MahppyBird where
        moveCameraBy :: (MonadState Vars m) => V2 CInt -> m ()
        moveCameraBy transform = vRenderingVars.cameraPos %= (+ (P transform))

        setCameraPos :: (MonadState Vars m) => Point V2 Float -> m ()
        setCameraPos (P npos) = vRenderingVars.cameraPos .= (P (roundV2 npos))

        getCameraPos :: (MonadState Vars m ) => m (Point V2 CInt)
        getCameraPos = use $ vRenderingVars.cameraPos

        getCameraOffset :: MonadState Vars m => m (V2 Float)
        getCameraOffset = use $ vRenderingVars.camOffset
