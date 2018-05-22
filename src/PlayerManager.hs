module PlayerManager where

import Linear.V2
import Aabb

class Monad m => PlayerManager m where
        getPlayerPos :: m (V2 Float)
        getPlayerAttributes :: m (V2 Float, V2 Float) -- position, V2 width height
        getPlayerVel :: m (Float)

        setPlayerPos :: V2 Float -> m ()
        setPlayerVel :: Float -> m ()

        getPlayerAabb :: m Aabb 
        resetPlayerPos :: m ()
