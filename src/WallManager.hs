module WallManager 
        where

import Walls
import Aabb
import Linear.V2

class Monad m => WallManager m where
        transformToWorldCoord :: Wall -> m ( (V2 Float, V2 Float) -- top left corner position of top wall, width / height
                                           , (V2 Float, V2 Float) )-- top left corner position of bottom wall, width /height
        getFirstWall :: m Wall
        getEntireFirstWallAabb :: m Aabb
        getFirstWallGapAabb :: m Aabb
        getWalls :: m ([Wall])
        popWall :: m ()
