{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module GuiTransforms where

import SDL
import SDL.Video 
import Control.Monad.Reader
import Control.Monad.State
import GameVars

class Monad m => GuiTransforms m where
        xCenterRectangle ::  Rectangle Float-> m (Rectangle Float)
        yCenterRectangle ::  Rectangle Float-> m (Rectangle Float)

instance GuiTransforms MahppyBird where
        xCenterRectangle :: (MonadReader Config m, MonadIO m) => Rectangle Float-> m (Rectangle Float)
        xCenterRectangle rect = do
                window <- asks cWindow 
                V2 winW winH <- (\(V2 a b)-> V2 (fromIntegral a ) (fromIntegral b)) <$> glGetDrawableSize window
                let Rectangle (P (V2 _ y)) (V2 width height) = rect
                    topleftpt = V2 (((winW) / 2) - (width / 2)) y
                    nrect = Rectangle (P topleftpt) (V2 width height) 
                return nrect

        yCenterRectangle :: (MonadReader Config m, MonadIO m) => Rectangle Float -> m (Rectangle Float)
        yCenterRectangle rect = do
                window <- asks cWindow 
                V2 winW winH <- (\(V2 a b)-> V2 (fromIntegral a ) (fromIntegral b)) <$> glGetDrawableSize window
                let Rectangle (P (V2 x _)) (V2 width height) = rect 
                    topleftpt = V2 x (((fromIntegral winH) / 2) - (height / 2))
                    nrect = Rectangle (P topleftpt) (V2 width height)
                return nrect


translate :: V2 Float -> Rectangle Float -> Rectangle Float
translate (V2 dx dy) (Rectangle (P (V2 x y)) lengths) = Rectangle (P (V2 (dx + x) (dy + y))) lengths
