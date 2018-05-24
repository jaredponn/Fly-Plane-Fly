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
        wallToSDLRect :: Wall -> m (SDL.Rectangle CInt, SDL.Rectangle CInt)

        -- "ToScreen" functions are unaffected by camera position
        drawRectToScreen :: SDL.Rectangle Float -> m ()

        -- wrapper for SDL.present renderer
        presentRenderer :: m()

        toScreenCord :: SDL.Point V2 Float -> m (SDL.Point V2 CInt)
        toScreenRect :: SDL.Rectangle Float -> m (SDL.Rectangle CInt)


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
                bgtexture <- bgTexture <$> asks cResources
                SDL.copy renderer bgtexture Nothing Nothing
                {- SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255 -}
                {- SDL.clear renderer -}

        drawPlayer :: (Renderer m, MonadIO m, MonadReader Config m, PlayerManager m, Renderer m) => m ()
        drawPlayer = do
                renderer <- asks cRenderer 
                playertexture <- playerTexture <$> asks cResources
                player <- getPlayer
                player' <- toScreenRect player
                SDL.copy renderer playertexture Nothing (Just player') -- TODO

        drawWalls :: (Renderer m, WallManager m, MonadIO m, MonadReader Config m) => m ()
        drawWalls = do
                renderer <- asks cRenderer 
                topwalltexture <- topWallTexture <$> asks cResources
                botwalltexture <- botWallTexture <$> asks cResources
                walls <- getWallsInScreen >>= mapM wallToSDLRect
                mapM_ (f renderer topwalltexture botwalltexture) walls
                where
                        f :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> SDL.Texture -> (SDL.Rectangle CInt,  SDL.Rectangle CInt) -> m ()
                        f renderer topwalltexture botwalltexture (top, bot)= do
                                SDL.copy renderer topwalltexture Nothing $ Just top
                                SDL.copy renderer botwalltexture Nothing $ Just bot
                                return ()

        -- uses the dimensions from the size of the screen and translates it so that it conforms to the specifcation of the Wall
        wallToSDLRect :: (Renderer m, WallManager m, MonadReader Config m, PlayerManager m) => Wall -> m (SDL.Rectangle CInt, SDL.Rectangle CInt)
        wallToSDLRect wall = do
                let wallwidth = wallWidth wall -- width of the wall
                (_, wallheight) <- asks cWindowSize -- height of the wall
                let lengths = V2 (round wallwidth) wallheight

                topPoint <- toScreenCord . SDL.P $ V2 (xPos wall) (0 - (gap wall + lowerWall wall))
                botPoint <- toScreenCord . SDL.P $ V2 (xPos wall) (upperWall wall + gap wall)

                return $ ( SDL.Rectangle topPoint lengths
                         , SDL.Rectangle botPoint lengths )

        -- draws it directly to the screen irregardless of th ecamera coordinate
        drawRectToScreen :: (Renderer m, MonadIO m, MonadReader Config m, PlayerManager m) => SDL.Rectangle Float -> m ()
        drawRectToScreen (SDL.Rectangle (SDL.P pos) lengths) = do
                renderer <- asks cRenderer 
                SDL.rendererDrawColor renderer $= SDL.V4 0 0 255 255
                {- let pos' = roundV2 pos -}
                let pos' = roundV2 pos
                    lengths' = roundV2 lengths
                SDL.fillRect renderer . Just $ SDL.Rectangle (SDL.P pos') lengths'

        presentRenderer :: (MonadReader Config m, MonadIO m) => m ()
        presentRenderer = asks cRenderer >>= SDL.present

        toScreenCord :: (CameraManager m) => SDL.Point V2 Float -> m (SDL.Point V2 CInt)
        toScreenCord (SDL.P pos) = do
                let pos' = roundV2 pos
                SDL.P cam <- getCamera
                return . SDL.P $ pos' - cam

        toScreenRect :: (Renderer m) => SDL.Rectangle Float -> m (SDL.Rectangle CInt)
        toScreenRect (SDL.Rectangle pos lengths) = do
                pos' <- toScreenCord pos
                let lengths' = roundV2 lengths
                return $ SDL.Rectangle pos' lengths'

