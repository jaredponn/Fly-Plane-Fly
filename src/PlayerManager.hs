module PlayerManager where

import Linear.V2
import Aabb

class Monad m => PlayerManager m where
        getPlayerPos :: m (V2 Float)
        getPlayerAabb :: m Aabb 
