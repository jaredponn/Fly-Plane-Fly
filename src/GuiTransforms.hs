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

        alignToLeftEdge :: Rectangle Float -> m (Rectangle Float)
        alignToRightEdge :: Rectangle Float -> m (Rectangle Float)

        getWindowSize :: m (V2 Float)

instance GuiTransforms MahppyBird where
        getWindowSize :: (MonadReader Config m, MonadIO m) => m (V2 Float)
        getWindowSize = do
                window <- asks cWindow 
                (\(V2 a b)-> V2 (fromIntegral a ) (fromIntegral b)) <$> glGetDrawableSize window

        xCenterRectangle :: GuiTransforms m => Rectangle Float-> m (Rectangle Float)
        xCenterRectangle rect = do
                V2 winW winH <- getWindowSize
                let Rectangle (P (V2 _ y)) (V2 width height) = rect
                    topleftpt = V2 ((winW / 2) - (width / 2)) y
                    nrect = Rectangle (P topleftpt) (V2 width height) 
                return nrect

        yCenterRectangle :: GuiTransforms m => Rectangle Float -> m (Rectangle Float)
        yCenterRectangle rect = do
                V2 winW winH <- getWindowSize
                let Rectangle (P (V2 x _)) (V2 width height) = rect 
                    topleftpt = V2 x ((winH / 2) - (height / 2))
                    nrect = Rectangle (P topleftpt) (V2 width height)
                return nrect

        alignToLeftEdge :: (MonadReader Config m, MonadIO m) => Rectangle Float -> m (Rectangle Float)
        alignToLeftEdge rect = do
                let Rectangle (P (V2 _ y)) (V2 width height) = rect
                    topleftpt = (P (V2 0 y))
                    nrect = Rectangle topleftpt (V2 width height)
                return nrect

        alignToRightEdge :: GuiTransforms m => Rectangle Float -> m (Rectangle Float)
        alignToRightEdge rect = do
                V2 winW winH <- getWindowSize
                let Rectangle (P (V2 _ y)) (V2 width height) = rect
                    topleftpt = (P (V2 (winW - width) y))
                    nrect = Rectangle topleftpt (V2 width height)
                return nrect

translate :: V2 Float -> Rectangle Float -> Rectangle Float
translate (V2 dx dy) (Rectangle (P (V2 x y)) lengths) = Rectangle (P (V2 (dx + x) (dy + y))) lengths
