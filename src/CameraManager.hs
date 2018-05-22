module CameraManager where

import Linear.V2
import Foreign.C.Types

class Monad m => CameraManager m where
        getCameraOffset :: m (V2 Float)

        setCameraPos :: V2 Float -> m ()
        moveCameraBy :: V2 CInt -> m ()

        getCamera :: m (V2 CInt)
