module Renderer where

import Foreign.C.Types
import Walls
import SDL 

type Time = Float

class Monad m => Renderer m where
        drawScreen :: m ()
        drawBg :: m ()
        drawPlayer :: m ()

        drawWalls :: m ()
        wallToSDLRect :: Wall -> m ([Rectangle CInt])

        -- Just used for debugging
        drawRect :: (V2 Float, V2 Float) -> m ()

        -- wrapper for SDL.present renderer
        presentRenderer :: m()

        moveCameraBy :: V2 CInt -> m ()
        setCameraPos :: V2 Float -> m ()
        toScreenCord :: V2 Float -> m (V2 CInt)

roundCoord :: V2 Float -> V2 CInt
roundCoord (V2 a b) = V2 (CInt $ round a) (CInt $ round b)
