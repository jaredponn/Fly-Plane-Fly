{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module CameraManager where

import Linear.V2
import Foreign.C.Types
import Control.Monad.Reader
import Control.Monad.State

import GameVars
import Util

class Monad m => CameraManager m where
        getCameraOffset :: m (V2 Float)

        setCameraPos :: V2 Float -> m ()
        moveCameraBy :: V2 CInt -> m ()

        getCamera :: m (V2 CInt)

instance CameraManager MahppyBird where
        moveCameraBy :: (MonadState Vars m) => V2 CInt -> m ()
        moveCameraBy transform = do
                cam <- gets camera 
                modify (\v -> v { camera  = cam + transform } )

        setCameraPos :: (MonadState Vars m) => V2 Float -> m ()
        setCameraPos cam = do
                modify (\v -> v { camera  = roundV2 cam } )


        getCamera :: (MonadState Vars m ) => m (V2 CInt)
        getCamera = gets camera

        getCameraOffset :: MonadState Vars m => m (V2 Float)
        getCameraOffset = gets cCamOffset
