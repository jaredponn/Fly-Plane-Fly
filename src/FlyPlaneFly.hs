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
        curscenestate <- viewSceneState
        runScene curscenestate

        unless (curscenestate == Quit) loop 

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
        input <- getInput

        -- setting up the play button
        playbtntexture <- view $ cResources.cTextures.guiTextures.playBtnTexture
        playbtnattr <- translateButtonAttr (V2 (-30) 0 ) <$> createRightEdgeAlignedButtonAttr 150 (V2 600 300) playbtntexture
        runReaderT (playbtneffect input) playbtnattr -- running the play button effect

        -- setting up the quit button
        quitbtntexture <- view $ cResources.cTextures.guiTextures.quitBtnTexture
        quitbtnattr <- translateButtonAttr (V2 (-30) 0 ) <$> createRightEdgeAlignedButtonAttr 500 (V2 600 100) quitbtntexture
        runReaderT (quitbtneffect input) quitbtnattr -- running the quit button effect

        -- setting up the mute button
        mutebtntexture <- getMuteBtnTexture
        mutebtnattr <- translateButtonAttr (V2 (-10) (-10)) <$> ((createRightEdgeAlignedButtonAttr 0 (V2 20 20) mutebtntexture) >>= alignToBottomEdgeButtonAttr)
        runReaderT (mutebtneffect input) mutebtnattr -- running the mute button effect

        -- updates 
        updateCameraPos -- makes the player not appear at the edge of the screen
        updatePlayerAnimation 

        titlebgpic <- view $ cResources.cTextures.guiTextures.titleScreenbg
        drawObjectsWithDt [drawBg titlebgpic, drawWalls, drawPlayer, drawBtnToScreen playbtnattr, drawBtnToScreen quitbtnattr, drawBtnToScreen mutebtnattr]

        where
                playbtneffect ::( SceneStateManager m
                                , HasInput m
                                , PlayerManager m ) => Input -> Button m
                playbtneffect = buttonEffectFromMouse $ setSceneState PrePlay

                quitbtneffect :: ( SceneStateManager m
                                 , HasInput m ) => Input -> Button m
                quitbtneffect = buttonEffectFromMouse $ setSceneState Quit

                mutebtneffect :: ( SoundManager m
                                 , HasInput m
                                 , Logger m) => Input -> Button m
                mutebtneffect = buttonEffectFromMouse pauseOrPlaySounds

runScene PrePlay = do
        input <- getInput

        -- setting up the press space to play rectangle
        pressspacetoplay <- view $ cResources.cTextures.guiTextures.pressSpacetoJumpTexture
        pressspacetoplayrect <- (xCenterRectangle >=> yCenterRectangle) $ SDL.Rectangle (SDL.P (V2 0 0)) (V2 550 194)

        -- getting the bg texture
        bgpic <- view $ cResources.cTextures.bgTexture

        drawObjectsWithDt [drawBg bgpic, drawPlayer, drawWalls, drawTextureToScreen pressspacetoplayrect pressspacetoplay]

        -- updates
        updateCameraPos
        updatePlayerAnimation

        -- if space is pressed, start the game
        if _isSpace input
           then do
                   jumpPlayer
                   setSceneState Play
           else return ()


runScene Play = do
        input <- getInput

        -- getting the bg picture
        bgpic <- view $ cResources.cTextures.bgTexture

        drawObjectsWithDt [drawBg bgpic, drawScore, drawWalls, drawPlayer]

        -- input response
        inputHandler input

        -- updates
        updateCameraPos
        updatePlayerAngle
        updateAnimations'
        updatePhysics
        collisionTest
        updateWalls
        updateScore

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

                        -- stops the jump animation if the player velocity is less than 0 (not jumping)
                        playeryvel <- getPlayerYVel
                        if playeryvel > 0
                                then removePlayerAnimationsUpto AnimationType'Idle
                                else return ()


                -- removes the front wall if it is outside of the screen
                updateWalls :: (PlayerManager m, WallManager m, CameraManager m) => m ()
                updateWalls = do
                        SDL.P (V2 x _ )<- getPlayerPos 
                        fstWall <- getFirstWall
                        wallwidth <- wallWidth <$> getFirstWall
                        V2 xcamoffset _ <- getCameraOffset
                        if xPos fstWall <= x - (wallwidth + abs xcamoffset)
                           then popWall_
                           else return ()

                -- increments the score if the player passes the gap
                updateScore :: (Logger m, PlayerManager m, WallManager m, ScoreManager m) => m ()
                updateScore = do
                        playerpos <- getPlayerPos 
                        gapaabb <- getFirstWallGapAabb
                        prevpassingwall <- getIsPassingWall
                        -- increments the score when the last frame was hit-testing with the gap and the current frame is not hit-testing with the gap
                        if not (pointHitTest playerpos gapaabb) && prevpassingwall
                           then incrementScore
                           else return ()
                        setIsPassingWall (pointHitTest playerpos gapaabb)

                collisionTest :: (SoundManager m, AnimationsManager m, WallManager m, PlayerManager m, Logger m, ScoreManager m, SceneStateManager m) => m ()
                collisionTest = do
                        playerAabb <- getPlayerAabb

                        -- the walls' aabbs are shifted and made smaller so it matches the graphic size
                        upperWallAabb <- addCushiontoAabb (V2 (-10) 0) <$> shiftAabb (V2 0 (-15)) <$> floorAabb <$> getFirstUpperWallAabb
                        lowerWallAabb <- addCushiontoAabb (V2 (-10) 0) <$> shiftAabb (V2 0 (15)) <$> ceilingAabb <$> getFirstLowerWallAabb
                        if (hitTestAbove playerAabb upperWallAabb) || (hitTestBelow playerAabb lowerWallAabb)
                        then do 
                                curscore <- getScore
                                ishighscore <- isHighScore curscore
                                if ishighscore 
                                   then setHighScore curscore
                                   else return ()

                                replacePlayerAnimation =<< getPlayerDeathAnimation 
                                playCrashFx
                                setSceneState GameOver

                        else return ()

runScene Pause = do
        input <- getInput

        -- setting up the mute button
        mutebtntexture <- getMuteBtnTexture
        mutebtnattr <- translateButtonAttr (V2 (-10) (-10)) <$> ((createRightEdgeAlignedButtonAttr 0 (V2 20 20) mutebtntexture) >>= alignToBottomEdgeButtonAttr)
        runReaderT (mutebtneffect input) mutebtnattr

        updateCameraPos

        font <- view $ cResources.cFont.scoreFont -- the "paused" text will use the same font as the score
        bgpic <- view $ cResources.cTextures.bgTexture

        drawObjectsWithDt [drawBg bgpic, drawScore, drawWalls, drawPlayer, drawScreenOverlay $ SDL.V4 0 0 0 95, drawTextToScreen font "paused" (SDL.P $ SDL.V2 0 0) (SDL.V4 155 98 0 100) (yCenterRectangle >=> xCenterRectangle), drawBtnToScreen mutebtnattr]

        if _isEsc input || _isSpace input
           then setSceneState Play
           else return ()

        where
                mutebtneffect :: ( SoundManager m
                                 , HasInput m
                                 , Logger m ) => Input -> Button m
                mutebtneffect = buttonEffectFromMouse pauseOrPlaySounds

runScene GameOver = do
        input <- getInput

        -- setting up the play again button
        playagainbtntexture <- view $ cResources.cTextures.guiTextures.playAgainBtnTexture
        playagainbtnattr <- translateButtonAttr (V2 (-125) 150) <$> createCenteredButtonAttr (V2 200 50) playagainbtntexture
        runReaderT (playagainbtneffect input) playagainbtnattr -- running the play again button effect

        -- setting up the quit button
        quitbtntexture <- view $ cResources.cTextures.guiTextures.quitGameOverBtnTexture
        quitbtnattr <- translateButtonAttr (V2 (125) 150) <$> createCenteredButtonAttr (V2 200 50) quitbtntexture
        runReaderT (quitbtneffect input) quitbtnattr -- running the quit button effect

        -- setting up the mute button
        mutebtntexture <- getMuteBtnTexture
        mutebtnattr <- translateButtonAttr (V2 (-10) (-10)) <$> (((createRightEdgeAlignedButtonAttr 0 (V2 20 20) mutebtntexture) >>= alignToBottomEdgeButtonAttr))
        runReaderT (mutebtneffect input) mutebtnattr -- running the mute button effect

        -- setting up the game over window
        gameoverwindowtexture <- view $ cResources.cTextures.guiTextures.gameOverWindowTexture
        gameoverwindowrect <- GuiTransforms.translate (V2 0 (-50)) <$> ((xCenterRectangle >=> yCenterRectangle) (SDL.Rectangle (SDL.P (V2 0 0)) (V2 450 300)))

        updateCameraPos
        updatePlayerAnimation
        bgpic <- view $ cResources.cTextures.bgTexture

        drawObjectsWithDt [drawBg bgpic, drawScore, drawWalls, drawPlayer, drawBtnToScreen playagainbtnattr, drawBtnToScreen quitbtnattr, drawTextureToScreen gameoverwindowrect gameoverwindowtexture, drawBtnToScreen mutebtnattr, drawHighScores]

        where
                playagainbtneffect :: ( SceneStateManager m
                                      , WallManager m
                                      , PlayerManager m
                                      , AnimationsManager m
                                      , HasInput m
                                      , ScoreManager m
                                      , SoundManager m) => Input -> Button m
                playagainbtneffect = buttonEffectFromMouse $ setSceneState PrePlay >> resetGame

                quitbtneffect ::( SceneStateManager m
                                , SoundManager m) => Input -> Button m
                quitbtneffect = buttonEffectFromMouse $ setSceneState Quit

                mutebtneffect :: SoundManager m => Input -> Button m
                mutebtneffect = buttonEffectFromMouse pauseOrPlaySounds

runScene Quit = return ()
            
resetGame :: (WallManager m, PlayerManager m, ScoreManager m, AnimationsManager m) => m ()
resetGame = do
        resetScore
        changeWallConfStartingPosition 350 -- moves the walls to start closer to the player.
        resetWalls
        resetPlayerPos
        resetPlayerAngle
        setIsPassingWall False
        replacePlayerAnimation =<< getPlayerIdleAnimation
        setPlayerYVel 0


