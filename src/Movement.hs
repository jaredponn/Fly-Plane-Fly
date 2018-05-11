module Movement where

import Linear.V2

class Monad m => HasMovement m where
        translate :: V2 Float -> m ()
