module WallManager 
        where

import Walls
import Aabb
import Linear.V2

class Monad m => WallManager m where
        -- DEPRECATED
        transformToWorldCoord :: Wall -> m ( (V2 Float, V2 Float) -- top left corner position of top wall, width / height
                                           , (V2 Float, V2 Float) )-- top left corner position of bottom wall, width /height
        -- takes the percent values that the wall has and converts them into the sizes corrosponding to the window size in world coordinateS
        transformWallLengthsToWorldVals :: Wall -> m Wall
        getFirstWall :: m Wall

        getFirstUpperWallAabb :: m Aabb
        getFirstLowerWallAabb :: m Aabb
        getEntireFirstWallAabb :: m Aabb -- DEPRECATED
        getFirstWallGapAabb :: m Aabb -- DEPRECATED

        getWalls :: m ([Wall])
        popWall :: m ()
