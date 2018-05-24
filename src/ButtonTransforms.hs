{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module ButtonTransforms where

import Control.Monad.State.Lazy
import Control.Monad.Reader
import Linear.V2
import SDL (Rectangle (..)
           , Point (..)
           , Texture (..))

import Aabb
import GameVars
import Logger
import Util

data ButtonAttr = ButtonAttr { rect :: Rectangle Float
                             , aabb :: Aabb
                             , texture :: Texture}

type Button m = ReaderT (ButtonAttr) m ()



createButtonAttrFromAabb :: Aabb -> ButtonAttr
createButtonAttrFromAabb aabb = ButtonAttr { rect = aabbToRectangle aabb
                                            , aabb = aabb }

createButtonAttrFromRectangle :: Rectangle Float -> ButtonAttr
createButtonAttrFromRectangle nrect = ButtonAttr { rect = nrect 
                                         , aabb = rectangleToAabb nrect }

createXCenteredButtonAttr :: (ButtonTransforms m) => Float  -- y position
                          -> V2 Float -- lengths.  Width, height
                          -> m ButtonAttr
createXCenteredButtonAttr ypos lengths = do
        let tmpbtnattr = ButtonAttr { rect = Rectangle (P (V2 0 ypos)) lengths
                                    , aabb = Aabb (V2 0 0) (V2 0 0)}
        xCenterButton tmpbtnattr 

class Monad m => ButtonTransforms m where
        xCenterButton :: ButtonAttr -> m (ButtonAttr)
        yCenterButton :: ButtonAttr -> m (ButtonAttr)


instance ButtonTransforms MahppyBird where
        xCenterButton :: (Logger m, MonadReader Config m) => ButtonAttr -> m (ButtonAttr)
        xCenterButton button = do
                (winW, winH) <- (\(a, b) -> (fromIntegral a, fromIntegral b)) <$> asks cWindowSize
                let Rectangle (P (V2 _ y)) (V2 width height) = rect button
                    topleftpt = V2 (((winW) / 2) - (width / 2)) y
                    nrect = Rectangle (P topleftpt) (V2 width height) 
                return ButtonAttr { rect = nrect
                              , aabb = rectangleToAabb nrect }

        yCenterButton :: MonadReader Config m => ButtonAttr -> m (ButtonAttr)
        yCenterButton button = do
                (winW, winH) <- asks cWindowSize
                let Rectangle (P (V2 x _)) (V2 width height) = rect $ button
                    topleftpt = V2 x (((fromIntegral winH) / 2) - (height / 2))
                    nrect = Rectangle (P topleftpt) (V2 width height)
                return ButtonAttr { rect = nrect
                              , aabb = rectangleToAabb nrect }


