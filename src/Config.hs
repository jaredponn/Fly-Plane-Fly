module Config where

import qualified SDL
import qualified SDL.Font as TTF
import Foreign.C.Types

data Config = Config { cWindow :: SDL.Window
                     , cRenderer :: SDL.Renderer
                     , cResources :: Resources
                     , cWindowSize :: {-# UNPACK #-} !(CInt,CInt) --width, height
                     }

data Resources = Resources { cFont :: TTF.Font }

