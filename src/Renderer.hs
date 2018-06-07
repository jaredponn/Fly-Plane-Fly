{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE BangPatterns #-} 
{-# LANGUAGE OverloadedStrings #-}
module Renderer where

import Control.Monad.Reader
import Control.Lens
import Control.Monad.State
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.Types
import Linear.V2
import GHC.Word
import qualified SDL 
import qualified SDL.Font as TTF
import Data.StateVar (($=))
import System.Clock
import qualified Data.Text as T
{- import qualified System.Mem -}

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

        drawBg :: SDL.Texture-> m ()
        drawPlayer :: m ()
        drawScore :: m ()

        drawWalls :: m ()
        wallToSDLRect :: Wall -> m (SDL.Rectangle CInt, SDL.Rectangle CInt)

        drawScreenOverlay :: SDL.V4 Word8 -> m ()

        -- "ToScreen" functions are unaffected by camera position
        drawRectToScreen :: SDL.Rectangle Float -> m ()
        drawTextToScreen :: TTF.Font
                         -> T.Text  -- text to render
                         -> SDL.Point V2 Float  -- position
                         -> SDL.V4 GHC.Word.Word8 -- color
                         -> (SDL.Rectangle Float -> m (SDL.Rectangle Float)) -- transform to apply to the text
                         -> m ()
        drawBtnToScreen :: ButtonAttr -> m ()
        drawTextureToScreen :: SDL.Rectangle Float -> SDL.Texture -> m ()

        -- wrapper for SDL.present renderer
        presentRenderer :: m()

        toScreenCord :: SDL.Point V2 Float -> m (SDL.Point V2 CInt)
        toScreenRect :: SDL.Rectangle Float -> m (SDL.Rectangle CInt)


instance Renderer MahppyBird where
        drawObjects :: (Logger m, Renderer m, TimeManager m, MonadIO m) => [m ()] -> m ()
        drawObjects drawactions = do
                {- liftIO System.Mem.performGC -}
                threadDelay 3000 -- fixes the weird random speed ups / slow downs and maximum CPU usage
                mapM_ id drawactions
                presentRenderer

        drawObjectsWithDt :: (Logger m, Renderer m, TimeManager m) => [m ()] -> m ()
        drawObjectsWithDt drawactions = do
                t0 <- getRealTime 
                drawObjects drawactions
                t1 <- getRealTime
                setdt . convertToSeconds $ System.Clock.diffTimeSpec t1 t0

        drawBg :: (Renderer m, MonadIO m, MonadReader Config m, MonadState Vars m) => SDL.Texture -> m ()
        drawBg bgtexture = do
                renderer <- asks cRenderer 
                SDL.copy renderer bgtexture Nothing Nothing 

        drawScreenOverlay :: (Renderer m, MonadIO m, MonadReader Config m, MonadState Vars m, GuiTransforms m) => SDL.V4 Word8
                          -> m ()
        drawScreenOverlay !color = do
                renderer <- asks cRenderer 
                lengths <- roundV2 <$> getWindowSize
                SDL.rendererDrawColor renderer $= color
                SDL.fillRect renderer (Just $ SDL.Rectangle (SDL.P (V2 0 0)) lengths)


        drawPlayer :: (Logger m, Renderer m, MonadIO m, MonadReader Config m, PlayerManager m, Renderer m, AnimationsManager m) => m ()
        drawPlayer = do
                renderer <- asks cRenderer 
                playerspritesheet <- view $ cResources.cTextures.playerSpriteSheet

                curplayer <- getPlayerAttributes
                curplayer' <- toScreenRect curplayer

                ang <- getPlayerAngle

                srcrect <- srcRect <$> getPlayerAnimationSrc

                SDL.copyEx renderer playerspritesheet (Just srcrect) (Just curplayer') ang Nothing (V2 False False)


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

        drawScore :: (ScoreManager m, Renderer m, GuiTransforms m, MonadReader Config m) => m ()
        drawScore = do
                score <- getScore
                font <- view $ cResources.cFont.scoreFont
                drawTextToScreen font (T.pack . show $ score) (SDL.P (V2 0 0)) (SDL.V4 54 55 74 255) f
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

        drawTextToScreen :: (MonadIO m, MonadReader Config m) => TTF.Font
                         -> T.Text 
                         -> SDL.Point V2 Float 
                         -> SDL.V4 GHC.Word.Word8 -- color
                         -> (SDL.Rectangle Float -> m (SDL.Rectangle Float))
                         -> m ()
        drawTextToScreen font str pos color f = do
                renderer <- asks cRenderer 
                curtexture <- TTF.blended font color str >>= SDL.createTextureFromSurface renderer
                (width, height) <- (\(a,b) -> (fromIntegral a, fromIntegral b)) <$> TTF.size font str
                SDL.Rectangle (SDL.P npos) lengths <- f $ SDL.Rectangle pos (V2 width height)
                let npos' = roundV2 npos
                    lengths' = roundV2 lengths
                SDL.copy renderer curtexture Nothing . Just $ SDL.Rectangle (SDL.P npos') lengths'

        drawBtnToScreen :: (MonadIO m, MonadReader Config m) => ButtonAttr -> m ()
        drawBtnToScreen btnattr = do
                renderer <- asks cRenderer
                let nrect =  roundSDLRect . rect $ btnattr
                SDL.copy renderer (texture btnattr) Nothing $ Just nrect

        drawTextureToScreen :: (MonadIO m, MonadReader Config m) => SDL.Rectangle Float -> SDL.Texture -> m ()
        drawTextureToScreen !nrect ntexture = do
                renderer <- asks cRenderer
                let nrect' = roundSDLRect nrect
                SDL.copy renderer ntexture Nothing $ Just nrect'


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

