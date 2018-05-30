{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE OverloadedStrings #-}
module Renderer where

import Control.Monad.Reader
import Control.Lens
import Control.Monad.State
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.Types
import Linear.V2
import qualified SDL 
import qualified SDL.Font as Font
import Data.StateVar (($=))
import System.Clock
import qualified Data.Text as T

import PlayerManager
import Buttons
import AnimationsManager
import Animations
import GuiTransforms
import Logger
import TimeManager
import ScoreManager
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
        drawScore :: m ()

        drawWalls :: m ()
        wallToSDLRect :: Wall -> m (SDL.Rectangle CInt, SDL.Rectangle CInt)

        -- "ToScreen" functions are unaffected by camera position
        drawRectToScreen :: SDL.Rectangle Float -> m ()
        drawTextToScreen :: T.Text  -- text to render
                         -> SDL.Point V2 Float  -- position
                         -> (SDL.Rectangle Float -> m (SDL.Rectangle Float)) -- transform to apply to the text
                         -> m ()
        drawBtnToScreen :: ButtonAttr -> m ()
        drawTextureToScreen :: SDL.Rectangle Float -> SDL.Texture -> m ()

        -- wrapper for SDL.present renderer
        presentRenderer :: m()

        toScreenCord :: SDL.Point V2 Float -> m (SDL.Point V2 CInt)
        toScreenRect :: SDL.Rectangle Float -> m (SDL.Rectangle CInt)


instance Renderer MahppyBird where
        drawObjects :: (Logger m, Renderer m, TimeManager m) => [m ()] -> m ()
        drawObjects drawactions = do
                threadDelay 2000 -- fixes the weird random speed ups / slow downs and maximum CPU usage
                mapM_ id drawactions
                presentRenderer

        drawObjectsWithDt :: (Logger m, Renderer m, TimeManager m) => [m ()] -> m ()
        drawObjectsWithDt drawactions = do
                t0 <- getRealTime 
                drawObjects drawactions
                threadDelay 2000 -- fixes the weird random speed ups / slow downs and maximum CPU usage
                t1 <- getRealTime
                setdt . convertToSeconds $ System.Clock.diffTimeSpec t1 t0

        drawBg :: (Renderer m, MonadIO m, MonadReader Config m, MonadState Vars m) => m ()
        drawBg = do
                renderer <- asks cRenderer 
                bgtexture <- view $ cResources.cTextures.bgTexture
                SDL.copy renderer bgtexture Nothing Nothing 
                {- for drawing blank color backgrounds:   -}
                {- SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255 -}
                {- SDL.clear renderer -}

        drawPlayer :: (Renderer m, MonadIO m, MonadReader Config m, PlayerManager m, Renderer m, AnimationsManager m) => m ()
        drawPlayer = do
                renderer <- asks cRenderer 
                playerspritesheet <- view $ cResources.cTextures.playerSpriteSheet

                player <- getPlayerAttributes
                player' <- toScreenRect player

                srcrect <- srcRect <$> getPlayerAnimationSrc

                SDL.copy renderer playerspritesheet (Just srcrect) (Just player')
                
        {- drawPlayer = do -}
        {-         renderer <- asks cRenderer  -}
        {-         playertexture <- playerTexture <$> asks cResources -}
        {-         player <- getPlayer -}
        {-         player' <- toScreenRect player -}
        {-         SDL.copy renderer playertexture Nothing (Just player') -}

        drawWalls :: (Renderer m, WallManager m, MonadIO m, MonadReader Config m) => m ()
        drawWalls = do
                renderer <- asks cRenderer 
                topwalltexture <- view $ cResources.cTextures.topWallTexture
                botwalltexture <- view $ cResources.cTextures.botWallTexture
                walls <- getWallsInScreen >>= mapM wallToSDLRect
                mapM_ (f renderer topwalltexture botwalltexture) walls
                where
                        f :: (MonadIO m) => SDL.Renderer -> SDL.Texture -> SDL.Texture -> (SDL.Rectangle CInt,  SDL.Rectangle CInt) -> m ()
                        f renderer topwalltexture botwalltexture (top, bot)= do
                                SDL.copy renderer topwalltexture Nothing $ Just top
                                SDL.copy renderer botwalltexture Nothing $ Just bot
                                return ()

        -- uses the dimensions from the size of the screen and translates it so that it conforms to the specification of the Wall
        wallToSDLRect :: (Renderer m, WallManager m, MonadReader Config m, PlayerManager m, MonadIO m) => Wall -> m (SDL.Rectangle CInt, SDL.Rectangle CInt)
        wallToSDLRect wall = do
                let wallwidth = wallWidth wall -- width of the wall
                window <- asks cWindow 
                V2 _ wallheight <- (\(V2 a b)-> V2 (fromIntegral a ) (fromIntegral b)) <$> SDL.glGetDrawableSize window
                let lengths = V2 (round wallwidth) wallheight

                topPoint <- toScreenCord . SDL.P $ V2 (xPos wall) (0 - (gap wall + lowerWall wall))
                botPoint <- toScreenCord . SDL.P $ V2 (xPos wall) (upperWall wall + gap wall)

                return $ ( SDL.Rectangle topPoint lengths
                         , SDL.Rectangle botPoint lengths )

        drawScore :: (ScoreManager m, Renderer m, GuiTransforms m) => m ()
        drawScore = do
                score <- getScore
                drawTextToScreen (T.pack . show $ score) (SDL.P (V2 0 0)) f
                where
                        f :: (GuiTransforms m) => SDL.Rectangle Float -> m (SDL.Rectangle Float)
                        f rect = xCenterRectangle rect >>= yCenterRectangle 

        drawRectToScreen :: (Renderer m, MonadIO m, MonadReader Config m, PlayerManager m) => SDL.Rectangle Float -> m ()
        drawRectToScreen (SDL.Rectangle (SDL.P pos) lengths) = do
                renderer <- asks cRenderer 
                SDL.rendererDrawColor renderer $= SDL.V4 0 0 255 255
                let pos' = roundV2 pos
                    lengths' = roundV2 lengths
                SDL.fillRect renderer . Just $ SDL.Rectangle (SDL.P pos') lengths'

        drawTextToScreen :: (MonadIO m, MonadReader Config m) => T.Text 
                         -> SDL.Point V2 Float 
                         -> (SDL.Rectangle Float -> m (SDL.Rectangle Float))
                         -> m ()
        drawTextToScreen str pos f = do
                renderer <- asks cRenderer 
                font <- view $ cResources.cFont
                texture <- Font.blended font (SDL.V4 255 0 0 255) str >>= SDL.createTextureFromSurface renderer
                (width, height) <- (\(a,b) -> (fromIntegral a, fromIntegral b)) <$> Font.size font str
                SDL.Rectangle (SDL.P npos) lengths <- f $ SDL.Rectangle pos (V2 width height)
                let npos' = roundV2 npos
                    lengths' = roundV2 lengths
                SDL.copy renderer texture Nothing . Just $ SDL.Rectangle (SDL.P npos') lengths'

        drawBtnToScreen :: (MonadIO m, MonadReader Config m) => ButtonAttr -> m ()
        drawBtnToScreen btnattr = do
                renderer <- asks cRenderer
                let nrect =  roundSDLRect . rect $ btnattr
                SDL.copy renderer (texture btnattr) Nothing $ Just nrect

        drawTextureToScreen :: (MonadIO m, MonadReader Config m) => SDL.Rectangle Float -> SDL.Texture -> m ()
        drawTextureToScreen rect texture = do
                renderer <- asks cRenderer
                let nrect = roundSDLRect rect
                SDL.copy renderer texture Nothing $ Just nrect


        presentRenderer :: (MonadReader Config m, MonadIO m) => m ()
        presentRenderer = asks cRenderer >>= SDL.present

        toScreenCord :: (CameraManager m) => SDL.Point V2 Float -> m (SDL.Point V2 CInt)
        toScreenCord (SDL.P pos) = do
                let pos' = roundV2 pos
                SDL.P cam <- getCameraPos
                return . SDL.P $ pos' - cam

        toScreenRect :: (Renderer m) => SDL.Rectangle Float -> m (SDL.Rectangle CInt)
        toScreenRect (SDL.Rectangle pos lengths) = do
                pos' <- toScreenCord pos
                let lengths' = roundV2 lengths
                return $ SDL.Rectangle pos' lengths'

roundSDLRect :: SDL.Rectangle Float -> SDL.Rectangle CInt
roundSDLRect = (\(SDL.Rectangle (SDL.P (V2 x y)) (V2 w h)) -> SDL.Rectangle (SDL.P (V2 (round x) (round y))) (V2 (round w) (round h)))
