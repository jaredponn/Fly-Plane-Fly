{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE BangPatterns #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE OverloadedStrings #-} 
module FlyPlaneFly ( loop
                   , runFlyPlaneFly
                   ) where

import qualified SDL
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Control.Monad (unless)
import Linear.V2
import qualified Data.Text as T

import Aabb
import Buttons
import Animations (AnimationType (..))
import Walls
import Logger
import Input
import Renderer
import Physics
import GameVars
import WallManager
import PlayerManager
import AnimationsManager
import ScoreManager
import TimeManager
import SceneStateManager
import SoundManager
import CameraManager
import GuiTransforms

runFlyPlaneFly :: Config -> Vars -> FlyPlaneFly a -> IO a 
runFlyPlaneFly conf vars (FlyPlaneFly m) = evalStateT (runReaderT m conf) vars

loop :: ( Logger m
        , Renderer m
        , MonadReader Config m
        , HasInput m
        , Physics m
        , WallManager m
        , PlayerManager m
        , ScoreManager m
        , CameraManager m
        , MonadState Vars m
        , SoundManager m 
        , TimeManager m
        , GuiTransforms m
        , SceneStateManager m
        , AnimationsManager m) => m ()
loop = do
        updateInput
        curgamestate <- viewSceneState
        runScene curgamestate

        unless (curgamestate == Quit) loop 

runScene :: ( Logger m
            , MonadReader Config m
            , Renderer m
            , HasInput m
            , Physics m
            , WallManager m
            , PlayerManager m
            , ScoreManager m
            , CameraManager m
            , TimeManager m
            , GuiTransforms m 
            , SceneStateManager m
            , SoundManager m
            , AnimationsManager m) => SceneState -> m ()
runScene Menu = do
        playbtntexture <- view $ cResources.cTextures.guiTextures.playBtnTexture
        playbtnattr <- translateButtonAttr (V2 (-30) 0 ) <$> createRightEdgeAlignedButtonAttr 150 (V2 600 300) playbtntexture
        runReaderT playbtneffect playbtnattr

        quitbtntexture <- view $ cResources.cTextures.guiTextures.quitBtnTexture
        quitbtnattr <- translateButtonAttr (V2 (-30) 0 ) <$> createRightEdgeAlignedButtonAttr 500 (V2 600 100) quitbtntexture
        runReaderT quitbtneffect quitbtnattr

        arechannelsplaying <- areChannelsPlaying
        mutebtntexture <- if arechannelsplaying
                             then view $ cResources.cTextures.guiTextures.muteTexture
                             else view $ cResources.cTextures.guiTextures.mutedTexture


        mutebtnattr <- translateButtonAttr (V2 (-10) (-10)) <$> ((createRightEdgeAlignedButtonAttr 0 (V2 20 20) mutebtntexture) >>= alignToBottomEdgeButtonAttr)
        runReaderT mutebtneffect mutebtnattr

        updateCameraPos
        updatePlayerAnimation
        titlebgpic <- view $ cResources.cTextures.guiTextures.titleScreenbg

        drawObjectsWithDt [drawBg titlebgpic, drawWalls, drawPlayer, drawBtnToScreen playbtnattr, drawBtnToScreen quitbtnattr, drawBtnToScreen mutebtnattr]

        where
                playbtneffect ::( SceneStateManager m
                                , HasInput m
                                , PlayerManager m ) => Button m
                playbtneffect = buttonEffectFromMouse $ setSceneState PrePlay 

                quitbtneffect ::( SceneStateManager m
                                , HasInput m ) => Button m
                quitbtneffect = buttonEffectFromMouse $ setSceneState Quit

                mutebtneffect :: ( SoundManager m
                                       , HasInput m
                                       , Logger m) => Button m
                mutebtneffect = buttonEffectFromMouse pauseOrPlaySounds

runScene PrePlay = do
        input <- getInput
        pressspacetoplay <- view $ cResources.cTextures.guiTextures.pressSpacetoJumpTexture
        pressspacetoplayrect <- (xCenterRectangle >=> yCenterRectangle) $ SDL.Rectangle (SDL.P (V2 0 0)) (V2 550 194)

        updateCameraPos
        updatePlayerAnimation

        bgpic <- view $ cResources.cTextures.bgTexture

        drawObjectsWithDt [drawBg bgpic, drawPlayer, drawWalls, drawTextureToScreen pressspacetoplayrect pressspacetoplay]

        if _isSpace input
           then do
                   jumpPlayer
                   setGrav 2900
                   setSceneState Play
           else return ()


runScene Play = do
        input <- getInput

        updateCameraPos
        updatePlayerAngle
        updateAnimations'

        bgpic <- view $ cResources.cTextures.bgTexture
        drawObjectsWithDt [drawBg bgpic, drawScore, drawWalls, drawPlayer]

        inputHandler input
        updatePhysics

        collisionTest
        updateWalls
        updateScore

        return ()

        where
                inputHandler :: (MonadReader Config m ,AnimationsManager m, Physics m, PlayerManager m, TimeManager m, SoundManager m, SceneStateManager m) => Input -> m ()
                inputHandler input = do
                        if _isSpace input 
                           then do jumpPlayer
                                   join $ views (cResources.cAnimations.playerJumpAnimation) prependToPlayerAnimation 
                                   playJumpFx
                           else return ()

                        if _isEsc input
                           then pauseGame
                           else return ()

                updatePhysics :: Physics m => m ()
                updatePhysics = do
                        applyGrav
                        applyYVel
                        applyXVel

                pauseGame :: SceneStateManager m => m ()
                pauseGame = setSceneState Pause

                updateAnimations' :: (AnimationsManager m, PlayerManager m) => m ()
                updateAnimations' = do
                        updatePlayerAnimation
                        -- stops the jump animation if the player is not jumping
                        playeryvel <- getPlayerYVel
                        if playeryvel > 0
                                then removePlayerAnimationsUpto AnimationType'Idle
                                else return ()


                updateWalls :: (PlayerManager m, WallManager m, CameraManager m) => m ()
                updateWalls = do
                        SDL.P (V2 x _ )<- getPlayerPos 
                        fstWall <- getFirstWall
                        wallwidth <- wallWidth <$> getFirstWall
                        V2 xcamoffset _ <- getCameraOffset
                        if xPos fstWall <= x - (wallwidth + abs xcamoffset)
                           then popWall_
                           else return ()

                updateScore :: (Logger m, PlayerManager m, WallManager m, ScoreManager m) => m ()
                updateScore = do
                        playerpos <- getPlayerPos 
                        gapaabb <- getFirstWallGapAabb
                        prevpassingwall <- getIsPassingWall
                        if not (pointHitTest playerpos gapaabb) && prevpassingwall
                           then do incrementScore
                                   getScore >>= logText . show
                           else return ()
                        setIsPassingWall (pointHitTest playerpos gapaabb)

                collisionTest :: (SoundManager m, AnimationsManager m, WallManager m, PlayerManager m, Logger m, ScoreManager m, SceneStateManager m) => m ()
                collisionTest = do
                        playerAabb <- getPlayerAabb
                        upperWallAabb <- addCushiontoAabb (V2 (-10) 0) <$> shiftAabb (V2 0 (-15)) <$> floorAabb <$> getFirstUpperWallAabb
                        lowerWallAabb <- addCushiontoAabb (V2 (-10) 0) <$> shiftAabb (V2 0 (15)) <$> ceilingAabb <$> getFirstLowerWallAabb
                        if (hitTestAbove playerAabb upperWallAabb) || (hitTestBelow playerAabb lowerWallAabb)
                        then do 
                                curscore <- getScore
                                ishighscore <- isHighScore curscore
                                if ishighscore 
                                   then setHighScore curscore
                                   else return ()

                                getPlayerDeathAnimation >>= replacePlayerAnimation 
                                playCrashFx
                                setSceneState GameOver 

                        else return ()

runScene Pause = do
        input <- getInput
        arechannelsplaying <- areChannelsPlaying
        mutebtntexture <- if arechannelsplaying
                             then view $ cResources.cTextures.guiTextures.muteTexture
                             else view $ cResources.cTextures.guiTextures.mutedTexture
        mutebtnattr <- translateButtonAttr (V2 (-10) (-10)) <$> ((createRightEdgeAlignedButtonAttr 0 (V2 20 20) mutebtntexture) >>= alignToBottomEdgeButtonAttr)
        runReaderT mutebtneffect mutebtnattr

        updateCameraPos

        font <- view $ cResources.cFont.scoreFont
        bgpic <- view $ cResources.cTextures.bgTexture


        drawObjectsWithDt [drawBg bgpic, drawScore, drawWalls, drawPlayer, drawScreenOverlay $ SDL.V4 0 0 0 95, drawTextToScreen font "paused" (SDL.P $ SDL.V2 0 0) (SDL.V4 155 98 0 100) (yCenterRectangle >=> xCenterRectangle), drawBtnToScreen mutebtnattr]

        if _isEsc input || _isSpace input
           then setSceneState Play
           else return ()

        where
                mutebtneffect :: ( SoundManager m
                                       , HasInput m
                                       , Logger m) => Button m
                mutebtneffect = buttonEffectFromMouse pauseOrPlaySounds

runScene GameOver = do
        playagainbtntexture <- view $ cResources.cTextures.guiTextures.playAgainBtnTexture
        playagainbtnattr <- translateButtonAttr (V2 (-125) 150) <$> createCenteredButtonAttr (V2 200 50) playagainbtntexture
        runReaderT playagainbtneffect playagainbtnattr

        quitbtntexture <- view $ cResources.cTextures.guiTextures.quitGameOverBtnTexture
        quitbtnattr <- translateButtonAttr (V2 (125) 150) <$> createCenteredButtonAttr (V2 200 50) quitbtntexture
        runReaderT quitbtneffect quitbtnattr

        arechannelsplaying <- areChannelsPlaying
        mutebtntexture <- if arechannelsplaying
                             then view $ cResources.cTextures.guiTextures.muteTexture
                             else view $ cResources.cTextures.guiTextures.mutedTexture
        mutebtnattr <- translateButtonAttr (V2 (-10) (-10)) <$> (((createRightEdgeAlignedButtonAttr 0 (V2 20 20) mutebtntexture) >>= alignToBottomEdgeButtonAttr))
        runReaderT mutebtneffect mutebtnattr

        gameoverwindowtexture <- view $ cResources.cTextures.guiTextures.gameOverWindowTexture
        gameoverwindowrect <- GuiTransforms.translate (V2 0 (-50)) <$> ((xCenterRectangle >=> yCenterRectangle) (SDL.Rectangle (SDL.P (V2 0 0)) (V2 450 300)))


        updateCameraPos
        updatePlayerAnimation
        bgpic <- view $ cResources.cTextures.bgTexture

        drawObjectsWithDt [drawBg bgpic, drawScore, drawWalls, drawPlayer, drawBtnToScreen playagainbtnattr, drawBtnToScreen quitbtnattr, drawTextureToScreen gameoverwindowrect gameoverwindowtexture, drawBtnToScreen mutebtnattr, renderHighScores]

        where
                playagainbtneffect ::( SceneStateManager m
                                     , HasInput m
                                     , WallManager m
                                     , PlayerManager m
                                     , AnimationsManager m
                                     , ScoreManager m
                                     , SoundManager m) => Button m
                playagainbtneffect = buttonEffectFromMouse $ setSceneState PrePlay >> resetGame

                quitbtneffect ::( SceneStateManager m
                                , HasInput m
                                , SoundManager m) => Button m
                quitbtneffect = buttonEffectFromMouse $ setSceneState Quit

                mutebtneffect :: ( SoundManager m
                                       , HasInput m
                                       , Logger m) => Button m
                mutebtneffect = buttonEffectFromMouse pauseOrPlaySounds

runScene Quit = return ()


renderHighScores :: (MonadReader Config m, GuiTransforms m, ScoreManager m, Renderer m) => m ()
renderHighScores = do 
        highscore <- getHighScore
        highscorefont <- view $ cResources.cFont.highScoreFont
        drawTextToScreen highscorefont (T.pack . show $ highscore) (SDL.P (V2 0 0)) (SDL.V4 54 55 74 100) ((liftM (GuiTransforms.translate (V2 (0) (70)))) <$> (yCenterRectangle >=> xCenterRectangle))
        
        curscore <- getScore
        scorefont <- view $ cResources.cFont.scoreFont
        drawTextToScreen scorefont (T.pack . show $ curscore) (SDL.P (V2 0 0)) (SDL.V4 54 55 74 255) ((liftM (GuiTransforms.translate (V2 (140) (-70)))) <$> (yCenterRectangle >=> xCenterRectangle))


resetGame :: (WallManager m, PlayerManager m, ScoreManager m, AnimationsManager m) => m ()
resetGame = do
        resetScore
        changeWallConfStartingPosition 350
        resetWalls
        resetPlayerPos
        resetPlayerAngle
        setIsPassingWall False
        replacePlayerAnimation =<< getPlayerIdleAnimation
        setPlayerYVel 0

updatePlayerAngle :: PlayerManager m => m ()
updatePlayerAngle = do
        curyvel <- getPlayerYVel
        ang <- getPlayerAngle
        if curyvel < 0 && ang >= (-10)
           then setPlayerAngle (ang - 1)
           else if ang <= 0
           then setPlayerAngle (ang + 1)
           else return ()
            
resetPlayerPos :: (PlayerManager m) => m ()
resetPlayerPos = setPlayerPos $ SDL.P $ V2 0 260

resetPlayerAngle :: PlayerManager m => m ()
resetPlayerAngle = setPlayerAngle 0

-- if the button is pressed, then execute the modifier to the game state
buttonEffectFromMouse :: ( HasInput m ) => m ()  -- action to modify the game
                        -> Button m
buttonEffectFromMouse f = do
        mousepos <- lift $ (\(V2 a b) -> (SDL.P (V2 (fromIntegral a) (fromIntegral b)))) <$> _mousePos <$> getInput
        mousepress <- lift $ _mousePress <$> getInput

        btnattr <- ask 

        if pointHitTest mousepos (aabb btnattr) && mousepress
           then lift f
           else return () 

updateCameraPos :: (PlayerManager m, CameraManager m) => m ()
updateCameraPos = do
        SDL.P (V2 x _) <- getPlayerPos
        camoffset <- getCameraOffset
        setCameraPos . SDL.P $ (V2 x 0) + camoffset

pauseOrPlaySounds :: (SoundManager m, Logger m, HasInput m) => m ()
pauseOrPlaySounds = do
        arechannelsplaying <- areChannelsPlaying
        if not arechannelsplaying 
           then resumeAll
           else muteAll 
