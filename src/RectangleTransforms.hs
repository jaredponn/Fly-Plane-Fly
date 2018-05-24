{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 

module RectangleTransforms where

import SDL
import Control.Monad.Reader

import GameVars

class Monad m => RectangleTransforms m where
        xCenterRectangle ::  Rectangle Float-> m (Rectangle Float)
        yCenterRectangle ::  Rectangle Float-> m (Rectangle Float)


instance RectangleTransforms MahppyBird where
        xCenterRectangle :: (MonadReader Config m) => Rectangle Float-> m (Rectangle Float)
        xCenterRectangle rect = do
                (winW, winH) <- (\(a, b) -> (fromIntegral a, fromIntegral b)) <$> asks cWindowSize
                let Rectangle (P (V2 _ y)) (V2 width height) = rect
                    topleftpt = V2 (((winW) / 2) - (width / 2)) y
                    nrect = Rectangle (P topleftpt) (V2 width height) 
                return nrect

        yCenterRectangle :: MonadReader Config m => Rectangle Float -> m (Rectangle Float)
        yCenterRectangle rect = do
                (winW, winH) <- asks cWindowSize
                let Rectangle (P (V2 x _)) (V2 width height) = rect 
                    topleftpt = V2 x (((fromIntegral winH) / 2) - (height / 2))
                    nrect = Rectangle (P topleftpt) (V2 width height)
                return nrect


