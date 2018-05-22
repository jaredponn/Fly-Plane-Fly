module WallManager 
        where

import Walls
import Aabb
import Linear.V2

class Monad m => WallManager m where
        -- takes the percent values that the wall has and converts them into the sizes corrosponding to the window size in world coordinateS
        transformWallLengthsToWorldVals :: Wall -> m Wall

        getFirstUpperWallAabb :: m Aabb
        getFirstLowerWallAabb :: m Aabb

        getWallsInScreen :: m ([Wall])

        getFirstWall :: m Wall
        popWall :: m (Wall)
        popWall_ :: m ()

        resetWalls :: m ()
