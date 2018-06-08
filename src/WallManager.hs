{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module WallManager 
        where

import Linear.V2
import Control.Monad.Reader
import Control.Monad.State
import Data.Stream as S
import SDL
import Control.Lens

import Aabb
import Walls
import GameVars
import Logger

class Monad m => WallManager m where
        -- takes the percent values that the wall has and converts them into the sizes corrosponding to the window size in world coordinates
        -- all walls in the wall manager monadic context will be converted to the corrosponding window size in world coordinates
        transformWallLengthsToWorldVals :: Wall -> m Wall

        getFirstUpperWallAabb :: m Aabb
        getFirstLowerWallAabb :: m Aabb
        getFirstWallGapAabb :: m Aabb

        getWallsInScreen :: m ([Wall])

        getFirstWall :: m Wall
        popWall :: m (Wall)
        popWall_ :: m ()

        changeWallConfStartingPosition :: Float -> m ()
        resetWalls :: m ()

instance WallManager FlyPlaneFly where
        transformWallLengthsToWorldVals :: (MonadReader Config m, MonadIO m) => Wall -> m Wall
        transformWallLengthsToWorldVals wall = do
                window <- asks cWindow 
                V2 _ winH <- (\(V2 a b)-> V2 (fromIntegral a ) (fromIntegral b)) <$> glGetDrawableSize window
                return Wall { upperWall = winH * upperWall wall
                            , gap = winH * gap wall
                            , lowerWall = winH * lowerWall wall
                            , xPos = xPos wall
                            , wallWidth = wallWidth wall }

        getWallsInScreen :: (MonadState Vars m, MonadReader Config m, WallManager m, MonadIO m) => m ([Wall])
        getWallsInScreen = do
                window <- asks cWindow 
                V2 winW _ <- (\(V2 a b)-> V2 (fromIntegral a ) (fromIntegral b)) <$> glGetDrawableSize window
                wallconf <- use $ vPlayVars.wallConf
                wallstream <- use $ vPlayVars.wallStream
                let wallstorender = S.take (ceiling (winW / (allWallWidth wallconf + allWallSpacing wallconf))) wallstream
                mapM transformWallLengthsToWorldVals wallstorender

        getFirstWall :: (MonadState Vars m, WallManager m) => m (Wall)
        getFirstWall =  do
                wallstream <- use $ vPlayVars.wallStream
                transformWallLengthsToWorldVals . S.head $ wallstream

        getFirstUpperWallAabb :: WallManager m => m Aabb
        getFirstUpperWallAabb = do
                fstwall <- getFirstWall
                return $ Aabb (P (V2 (xPos fstwall) 0)) (P (V2 (wallWidth fstwall + xPos fstwall) (upperWall fstwall)))

        getFirstLowerWallAabb :: WallManager m => m Aabb
        getFirstLowerWallAabb = do
                fstwall <- getFirstWall
                return $ Aabb (P (V2 (xPos fstwall) (gap fstwall + upperWall fstwall))) (P (V2 (wallWidth fstwall + xPos fstwall) (upperWall fstwall + gap fstwall + lowerWall fstwall)))

        getFirstWallGapAabb :: (WallManager m) => m Aabb
        getFirstWallGapAabb = do
                fstwall <- getFirstWall
                return $ Aabb (P (V2 (xPos fstwall) (upperWall fstwall))) (P (V2 (xPos fstwall + wallWidth fstwall) (upperWall fstwall + gap fstwall + lowerWall fstwall)))

        popWall :: (MonadState Vars m, WallManager m) => m ( Wall )
        popWall = do
                fstwall <- getFirstWall
                wallstream <- use $ vPlayVars.wallStream
                vPlayVars.wallStream .=  S.tail wallstream
                return fstwall

        popWall_ :: (MonadState Vars m, WallManager m) => m ()
        popWall_ = do
                wallstream <- use $ vPlayVars.wallStream
                vPlayVars.wallStream .=  S.tail wallstream

        resetWalls :: (MonadState Vars m, MonadIO m, Logger m) => m ()
        resetWalls = do
                wallconf <- use $ vPlayVars.wallConf
                wallstream <- liftIO . createWallStream $ wallconf
                vPlayVars.wallStream .= wallstream

        changeWallConfStartingPosition :: MonadState Vars m => Float -> m ()
        changeWallConfStartingPosition newstartingpos = do
                wallconf <- use (vPlayVars.wallConf) 
                let nwallconf = wallconf {startingPos = newstartingpos}
                vPlayVars.wallConf .= nwallconf

