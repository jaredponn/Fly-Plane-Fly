module Renderer where

import Foreign.C.Types
import Walls
import SDL 

type Time = Float

class Monad m => Renderer m where
        drawObjects :: [m ()] -> m ()
        drawObjectsWithDt :: [m ()] -> m ()
        drawBg :: m ()
        drawPlayer :: m ()

        drawWalls :: m ()
        wallToSDLRect :: Wall -> m ([Rectangle CInt])

        -- "ToScreen" functions are unaffected by camera position
        drawRectToScreen :: (V2 Float, V2 Float) -> m ()

        -- wrapper for SDL.present renderer
        presentRenderer :: m()

        toScreenCord :: V2 Float -> m (V2 CInt)

roundV2 :: V2 Float -> V2 CInt
roundV2 (V2 a b) = V2 (CInt $ round a) (CInt $ round b)
