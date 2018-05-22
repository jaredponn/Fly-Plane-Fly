{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module Renderer where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.Types
import Linear.V2
import qualified SDL 
import Data.StateVar (($=))
import System.Clock

import PlayerManager
import Logger
import TimeManager
import CameraManager
import GameVars
import WallManager
import Util

import Walls

class Monad m => Renderer m where
        drawObjects :: [m ()] -> m ()
        drawObjectsWithDt :: [m ()] -> m ()
        drawBg :: m ()
        drawPlayer :: m ()

        drawWalls :: m ()
        wallToSDLRect :: Wall -> m ([SDL.Rectangle CInt])

        -- "ToScreen" functions are unaffected by camera position
        drawRectToScreen :: (V2 Float, V2 Float) -> m ()

        -- wrapper for SDL.present renderer
        presentRenderer :: m()

        toScreenCord :: V2 Float -> m (V2 CInt)


instance Renderer MahppyBird where
        drawObjects :: (Renderer m, TimeManager m) => [m ()] -> m ()
        drawObjects drawactions = do
                mapM_ id drawactions
                presentRenderer

        drawObjectsWithDt :: (Logger m, Renderer m, TimeManager m) => [m ()] -> m ()
        drawObjectsWithDt drawactions = do
                t0 <- getRealTime 
                drawObjects drawactions
                threadDelay 2000 -- fixes the weird random speed ups / slow downs
                t1 <- getRealTime
                setdt . convertToSeconds $ System.Clock.diffTimeSpec t1 t0

        drawBg :: (Renderer m, MonadIO m, MonadReader Config m, MonadState Vars m) => m ()
        drawBg = do
                renderer <- asks cRenderer 
                SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255
                SDL.clear renderer

        drawPlayer :: (Renderer m, MonadIO m, MonadReader Config m, PlayerManager m) => m ()
        drawPlayer = do
                renderer <- asks cRenderer 
                SDL.rendererDrawColor renderer $= SDL.V4 255 0 0 255
                (pPos, pSize) <- getPlayerAttributes
                pPos' <- toScreenCord pPos
                SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P pPos') (roundV2 pSize)

        drawWalls :: (Renderer m, WallManager m, MonadIO m, MonadReader Config m) => m ()
        drawWalls = do
                renderer <- asks cRenderer 
                SDL.rendererDrawColor renderer $= SDL.V4 0 255 0 255
                walls <- getWallsInScreen >>= mapM wallToSDLRect
                mapM_ (mapM_ (SDL.fillRect renderer . Just)) walls

        -- uses the dimensions from the size of the screen and translates it so that it conforms to the specifcation of the Wall
        wallToSDLRect :: (Renderer m, WallManager m, MonadReader Config m, PlayerManager m) => Wall -> m ([SDL.Rectangle CInt])
        wallToSDLRect wall = do
                let wallwidth = wallWidth wall -- width of the wall
                (_, wallheight) <- asks cWindowSize -- height of the wall
                let lengths = V2 (round wallwidth) wallheight

                topPoint <- toScreenCord $ V2 (xPos wall) (0 - (gap wall + lowerWall wall))
                botPoint <- toScreenCord $ V2 (xPos wall) (upperWall wall + gap wall)

                return $ [ SDL.Rectangle (SDL.P topPoint) lengths
                         , SDL.Rectangle (SDL.P botPoint) lengths ]

        drawRectToScreen :: (Renderer m, MonadIO m, MonadReader Config m, PlayerManager m) => (V2 Float, V2 Float) -> m ()
        drawRectToScreen (pos, lengths) = do
                renderer <- asks cRenderer 
                SDL.rendererDrawColor renderer $= SDL.V4 0 0 255 255
                let pos' = roundV2 pos
                let lengths' = (\(V2 a b) -> (V2 (round a) (round b))) lengths
                SDL.fillRect renderer . Just $ SDL.Rectangle (SDL.P pos') lengths'

        presentRenderer :: (MonadReader Config m, MonadIO m) => m ()
        presentRenderer = asks cRenderer >>= SDL.present

        toScreenCord :: (CameraManager m) => V2 Float -> m (V2 CInt)
        toScreenCord wPos = do
                let pos = roundV2 wPos
                cam <- getCamera
                return $ pos - cam
