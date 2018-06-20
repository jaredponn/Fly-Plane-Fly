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
import qualified Data.Text as T

import PlayerManager
import Buttons
import AnimationsManager
import Animations
import GuiTransforms
import Logger
import TimeManager
import SoundManager
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

        toScreenCord :: SDL.Point V2 Float -> m (SDL.Point V2 CInt)
        toScreenRect :: SDL.Rectangle Float -> m (SDL.Rectangle CInt)

        {- Wrappers:  -}
        -- SDL.present renderer
        presentRenderer :: m ()
        -- SDL.copy
        copy :: SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt) -> Maybe (SDL.Rectangle CInt) -> m ()
        -- SDL.copyEx
        copyEx :: SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt) -> Maybe (SDL.Rectangle CInt) -> CDouble -> Maybe (SDL.Point V2 CInt) -> V2 Bool -> m ()



instance Renderer FlyPlaneFly where
        drawObjects :: (Logger m, Renderer m, TimeManager m, MonadIO m) => [m ()] -> m ()
        drawObjects drawactions = do
                curdt <- getdt   

                -- this will delay the program such that it runs at a constant frame rate of no more than 64 frames per second
                -- it also will reduce the CPU usage of the program.
                threadDelay $ max (round . secsToMicroSecs $ 1/124 - milliSecsToSecs curdt) 0

                mapM_ id drawactions
                presentRenderer

        drawObjectsWithDt :: (Logger m, Renderer m, TimeManager m) => [m ()] -> m ()
        drawObjectsWithDt drawactions = do
                t0 <- getRealTime 
                drawObjects drawactions
                t1 <- getRealTime
                setdtFromTimeSpec t1 t0

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

        drawRectToScreen :: (Renderer m, MonadIO m, MonadReader Config m, PlayerManager m) => SDL.Rectangle Float -> m ()
        drawRectToScreen (SDL.Rectangle (SDL.P pos) lengths) = do
                renderer <- asks cRenderer 
                SDL.rendererDrawColor renderer $= SDL.V4 0 0 255 255
                let pos' = roundV2 pos
                    lengths' = roundV2 lengths
                SDL.fillRect renderer . Just $ SDL.Rectangle (SDL.P pos') lengths'

        drawTextToScreen :: (Renderer m, MonadReader Config m, MonadIO m) => TTF.Font
                         -> T.Text 
                         -> SDL.Point V2 Float 
                         -> SDL.V4 GHC.Word.Word8 -- color
                         -> (SDL.Rectangle Float -> m (SDL.Rectangle Float))
                         -> m ()
        drawTextToScreen font str pos color f = do
                renderer <- asks cRenderer 
                tmpsurface <- TTF.blended font color str
                curtexture <- SDL.createTextureFromSurface renderer tmpsurface
                SDL.freeSurface tmpsurface
                (width, height) <- (\(a,b) -> (fromIntegral a, fromIntegral b)) <$> TTF.size font str
                SDL.Rectangle (SDL.P npos) lengths <- f $ SDL.Rectangle pos (V2 width height)
                let npos' = roundV2 npos
                    lengths' = roundV2 lengths
                copy renderer curtexture Nothing . Just $ SDL.Rectangle (SDL.P npos') lengths'
                SDL.destroyTexture curtexture

        drawBtnToScreen :: (Renderer m, MonadReader Config m) => ButtonAttr -> m ()
        drawBtnToScreen btnattr = do
                renderer <- asks cRenderer
                let nrect =  roundSDLRect . rect $ btnattr
                copy renderer (texture btnattr) Nothing $ Just nrect

        drawTextureToScreen :: (Renderer m, MonadReader Config m) => SDL.Rectangle Float -> SDL.Texture -> m ()
        drawTextureToScreen !nrect ntexture = do
                renderer <- asks cRenderer
                let nrect' = roundSDLRect nrect
                copy renderer ntexture Nothing $ Just nrect'

        copy :: MonadIO m => SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt) -> Maybe (SDL.Rectangle CInt) -> m ()
        copy = SDL.copy

        copyEx :: MonadIO m => SDL.Renderer -> SDL.Texture -> Maybe (SDL.Rectangle CInt) -> Maybe (SDL.Rectangle CInt) -> CDouble -> Maybe (SDL.Point V2 CInt) -> V2 Bool -> m ()
        copyEx = SDL.copyEx

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

getMuteBtnTexture :: (MonadReader Config m, SoundManager m) => m (SDL.Texture)
getMuteBtnTexture = do
        arechannelsplaying <- areChannelsPlaying
        mutebtntexture <- if arechannelsplaying
                             then view $ cResources.cTextures.guiTextures.muteTexture
                             else view $ cResources.cTextures.guiTextures.mutedTexture
        return mutebtntexture

-- draws the score so it is centered in the screen
drawScore :: (ScoreManager m, Renderer m, GuiTransforms m, MonadReader Config m) => m ()
drawScore = do
        curscore <- getScore
        font <- view $ cResources.cFont.scoreFont
        drawTextToScreen font (T.pack . show $ curscore) (SDL.P (V2 0 0)) (SDL.V4 54 55 74 255) centerRectangle

-- draws the score and the highscore for the gameover scene state
drawHighScores :: (MonadReader Config m, GuiTransforms m, ScoreManager m, Renderer m) => m ()
drawHighScores = do 
        highscore <- getHighScore
        highscorefont <- view $ cResources.cFont.highScoreFont
        drawTextToScreen highscorefont (T.pack . show $ highscore) (SDL.P (V2 0 0)) (SDL.V4 54 55 74 100) ((liftM (GuiTransforms.translate (V2 (0) (70)))) <$> (yCenterRectangle >=> xCenterRectangle))
        
        curscore <- getScore
        scorefont <- view $ cResources.cFont.scoreFont
        drawTextToScreen scorefont (T.pack . show $ curscore) (SDL.P (V2 0 0)) (SDL.V4 54 55 74 255) ((liftM (GuiTransforms.translate (V2 (140) (-70)))) <$> (yCenterRectangle >=> xCenterRectangle))

drawPlayer :: (Logger m, Renderer m, MonadReader Config m, PlayerManager m, Renderer m, AnimationsManager m) => m ()
drawPlayer = do
        renderer <- asks cRenderer 
        playerspritesheet <- view $ cResources.cTextures.playerSpriteSheet

        curplayer <- getPlayerAttributes
        curplayer' <- toScreenRect curplayer

        ang <- getPlayerAngle

        srcrect <- srcRect <$> getPlayerAnimationSrc

        copyEx renderer playerspritesheet (Just srcrect) (Just curplayer') ang Nothing (V2 False False)

-- draws the walls in the screen
drawWalls :: (Renderer m, WallManager m, MonadReader Config m, PlayerManager m, GuiTransforms m) => m ()
drawWalls = do
        renderer <- asks cRenderer 
        topwalltexture <- view $ cResources.cTextures.topWallTexture
        botwalltexture <- view $ cResources.cTextures.botWallTexture
        walls <- getWallsInScreen >>= mapM wallToSDLRect
        mapM_ (f renderer topwalltexture botwalltexture) walls
                where
                        f :: Renderer m => SDL.Renderer -> SDL.Texture -> SDL.Texture -> (SDL.Rectangle CInt,  SDL.Rectangle CInt) -> m ()
                        f renderer topwalltexture botwalltexture (top, bot)= do
                                copy renderer topwalltexture Nothing $ Just top
                                copy renderer botwalltexture Nothing $ Just bot
                                return ()

-- uses the dimensions from the size of the screen and translates it so that it conforms to the specification of the Wall
wallToSDLRect :: (Renderer m, WallManager m, MonadReader Config m, PlayerManager m, GuiTransforms m) => Wall -> m (SDL.Rectangle CInt, SDL.Rectangle CInt)
wallToSDLRect wall = do
        let wallwidth = wallWidth wall -- width of the wall
        V2 _ wallheight <- getWindowSize
        let lengths = roundV2 $ V2 wallwidth wallheight

        topPoint <- toScreenCord . SDL.P $ V2 (xPos wall) (0 - (gap wall + lowerWall wall))
        botPoint <- toScreenCord . SDL.P $ V2 (xPos wall) (upperWall wall + gap wall)

        return $ ( SDL.Rectangle topPoint lengths
                 , SDL.Rectangle botPoint lengths )
