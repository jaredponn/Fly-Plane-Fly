module Renderer where

import Foreign.C.Types
import SDL 

type Time = Float

class Monad m => Renderer m where
        drawScreen :: m ()
        drawBg :: m ()
        drawPlayer :: m ()
        drawWall :: m ()

        -- wrapper for SDL.present renderer
        presentwr :: m()

        moveCameraBy :: V2 CInt -> m ()
        setCameraPos :: V2 Float -> m ()
        toScreenCord :: V2 Float -> m (V2 CInt)

roundCoord :: V2 Float -> V2 CInt
roundCoord (V2 a b) = V2 (CInt $ round a) (CInt $ round b)
