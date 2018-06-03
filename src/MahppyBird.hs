{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE BangPatterns #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE OverloadedStrings #-} 
module MahppyBird (MahppyBird (..)
            , loop
            , runMahppyBird
            ) where

import qualified SDL
import qualified SDL.Font as Font

import Data.StateVar (($=))
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens
import Foreign.C.Types
import Control.Monad (unless)
import Linear.V2
import Data.Stack
import qualified System.Clock
import qualified Data.Text as T

import Aabb
import Buttons
import Animations (AnimationType (..))
import Walls
import Logger
import Input
import Renderer
import Physics
import Walls
import GameVars
import WallManager
import PlayerManager
import AnimationsManager
import ScoreManager
import TimeManager
import GameStateManager
import SoundManager
import CameraManager
import GuiTransforms

runMahppyBird :: Config -> Vars -> MahppyBird a -> IO a 
runMahppyBird conf vars (MahppyBird m) = evalStateT (runReaderT m conf) vars

acquireInput :: (Logger m, HasInput m) => m Input
acquireInput = do
        updateInput
        getInput

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
        , GameStateManager m
        , AnimationsManager m) => m ()
loop = do
        input <- acquireInput
        curgamestate <- peekGameState

        runScene input curgamestate

        {- playState <- get -}
        {- logToFile "/home/jared/Programs/mahppybird/log.txt" . show $ playState -}

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
            , GameStateManager m
            , SoundManager m
            , AnimationsManager m) => Input -> GameState -> m ()
runScene input Menu = do
        (mousepress, mousepos) <- getMouseAttrib
        
        playbtntexture <- view $ cResources.cTextures.guiTextures.playBtnTexture
        playbtnattr <- translateButtonAttr (V2 (-30) 0 ) <$> createRightEdgeAlignedButtonAttr 150 (V2 600 300) playbtntexture
        runReaderT playbtneffect playbtnattr

        quitbtntexture <- view $ cResources.cTextures.guiTextures.quitBtnTexture
        quitbtnattr <- translateButtonAttr (V2 (-30) 0 ) <$> createRightEdgeAlignedButtonAttr 500 (V2 600 100) quitbtntexture
        runReaderT quitbtneffect quitbtnattr

        renderactions <- (++[drawBtnToScreen playbtnattr, drawBtnToScreen quitbtnattr]) <$> updatedRenderMenuActions  
        drawObjectsWithDt renderactions

        where
                playbtneffect ::( GameStateManager m
                                , HasInput m
                                , PlayerManager m ) => Button m
                playbtneffect = buttonGameStateModifierFromMouse $ pushGameState PrePlay 

                quitbtneffect ::( GameStateManager m
                                , HasInput m) => Button m
                quitbtneffect = buttonGameStateModifierFromMouse . pushGameState $ Quit

runScene input PrePlay = do
        pressspacetoplay <- view $ cResources.cTextures.guiTextures.pressSpacetoJumpTexture
        pressspacetoplayrect <- (xCenterRectangle >=> yCenterRectangle) $ SDL.Rectangle (SDL.P (V2 0 0)) (V2 550 194)
        pressspacetoplayrect <- (xCenterRectangle >=> yCenterRectangle) $ SDL.Rectangle (SDL.P (V2 0 0)) (V2 550 194)
        
        renderactions <- (++[drawTextureToScreen pressspacetoplayrect pressspacetoplay]) <$> updatedRenderPrePlayActions
        drawObjectsWithDt renderactions

        if _isSpace input 
           then do
                   jumpPlayer 
                   setGrav 2900 
                   popGameState_ 
                   pushGameState Play

           else return ()


runScene input Play = do
        renderactions <- updatedRenderPlayActions
        drawObjectsWithDt renderactions

        inputHandler input
        updatePhysics 

        collisionTest
        updateWalls
        updateScore
        where
                inputHandler :: (MonadReader Config m ,AnimationsManager m, Physics m, PlayerManager m, TimeManager m, SoundManager m, GameStateManager m) => Input -> m ()
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

                pauseGame :: GameStateManager m => m ()
                pauseGame = pushGameState Pause

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

                collisionTest :: (SoundManager m, AnimationsManager m, WallManager m, PlayerManager m, Logger m, ScoreManager m, GameStateManager m) => m ()
                collisionTest = do
                        playerAabb <- getPlayerAabb
                        upperWallAabb <- shiftAabb (V2 0 (-25)) <$> floorAabb <$> getFirstUpperWallAabb
                        lowerWallAabb <- shiftAabb (V2 0 (25)) <$> ceilingAabb <$> getFirstLowerWallAabb
                        if (hitTestAbove playerAabb upperWallAabb) || (hitTestBelow playerAabb lowerWallAabb)
                        then do 
                                curscore <- getScore
                                ishighscore <- isHighScore curscore
                                if ishighscore 
                                   then do
                                           logText $ "CONGRATS NEW HIGH SCORE: " ++ (show curscore)
                                           modifyHighScore curscore
                                           getHighScore >>= logText . show  
                                   else do
                                           logText $ "YOUR FINAL SCORE:" ++ (show curscore)

                                getPlayerDeathAnimation >>= replacePlayerAnimation 
                                playCrashFx
                                pushGameState (GameOver True)

                        else return ()

runScene input Pause = do
        renderactions <- updatedRenderPauseActions
        drawObjectsWithDt renderactions
        if _isEsc input || _isSpace input
           then popGameState_
           else return ()

runScene input (GameOver _) = do
        playagainbtntexture <- view $ cResources.cTextures.guiTextures.playAgainBtnTexture
        playagainbtnattr <- translateButtonAttr (V2 (-125) 150) <$> createCenteredButtonAttr (V2 200 50) playagainbtntexture
        runReaderT playagainbtneffect playagainbtnattr

        quitbtntexture <- view $ cResources.cTextures.guiTextures.quitGameOverBtnTexture
        quitbtnattr <- translateButtonAttr (V2 (125) 150) <$> createCenteredButtonAttr (V2 200 50) quitbtntexture
        runReaderT quitbtneffect quitbtnattr

        gameoverwindowtexture <- view $ cResources.cTextures.guiTextures.gameOverWindowTexture
        gameoverwindowrect <- GuiTransforms.translate (V2 0 (-50)) <$> ((xCenterRectangle >=> yCenterRectangle) (SDL.Rectangle (SDL.P (V2 0 0)) (V2 450 300)))

        renderactions <- (++[updatedRenderScoresAction]) <$> (++[drawBtnToScreen playagainbtnattr, drawBtnToScreen quitbtnattr, drawTextureToScreen gameoverwindowrect gameoverwindowtexture]) <$> updatedRenderGameOverActions

        drawObjectsWithDt renderactions

        where
                playagainbtneffect ::( GameStateManager m
                                     , HasInput m
                                     , WallManager m
                                     , PlayerManager m
                                     , AnimationsManager m
                                     , ScoreManager m) => Button m
                playagainbtneffect = buttonGameStateModifierFromMouse $ popGameState_ >> pushGameState PrePlay >> resetGame

                quitbtneffect ::( GameStateManager m
                                , HasInput m) => Button m
                quitbtneffect = buttonGameStateModifierFromMouse . pushGameState $ Quit

runScene input Quit = return ()


-- updatedRender<..> functions include some of the render functions necassary to render the game for that scene
updatedRenderMenuActions :: (AnimationsManager m, Renderer m, PlayerManager m, CameraManager m, MonadReader Config m, GuiTransforms m) => m [m()]
updatedRenderMenuActions = do
        updateCameraPos
        updatePlayerAnimation

        titletexttexture <- view $ cResources.cTextures.guiTextures.titleTextTexture
        titletextrect <- GuiTransforms.translate (V2 (30) 0) <$> (GuiTransforms.alignToLeftEdge $ SDL.Rectangle (SDL.P (V2 0 60)) (V2 590 551))

        return $ [drawBg, drawTextureToScreen titletextrect titletexttexture, drawWalls, drawPlayer]


updatedRenderPrePlayActions :: (AnimationsManager m, Renderer m, PlayerManager m, CameraManager m, GuiTransforms m, MonadReader Config m) => m ([m ()])
updatedRenderPrePlayActions = do
        updateCameraPos
        updatePlayerAnimation
        return [drawBg, drawPlayer, drawWalls] 

updatedRenderPlayActions :: (AnimationsManager m, Renderer m, PlayerManager m, CameraManager m) => m [m ()]
updatedRenderPlayActions = do
        updateCameraPos
        updatePlayerAngle
        updatePlayerAnimation
        -- stops the jump animation if the player is not jumping
        playeryvel <- getPlayerYVel
        if playeryvel > 0
           then removePlayerAnimationsUpto AnimationType'Idle
           else return ()

        return [drawBg, drawScore, drawWalls, drawPlayer]

        
updatedRenderPauseActions :: (Renderer m, PlayerManager m, CameraManager m) => m ([m ()])
updatedRenderPauseActions = do
        updateCameraPos
        return [drawBg, drawScore, drawWalls, drawPlayer] 

updatedRenderGameOverActions :: (AnimationsManager m, Renderer m, PlayerManager m, CameraManager m, MonadReader Config m, GuiTransforms m, ScoreManager m) => m [m ()]
updatedRenderGameOverActions = do
        updateCameraPos
        updatePlayerAngle
        updatePlayerAnimation
        return [drawBg, drawScore, drawWalls, drawPlayer] 

updatedRenderScoresAction :: (MonadReader Config m, GuiTransforms m, ScoreManager m, Renderer m) => m ()
updatedRenderScoresAction = do 
        highscore <- getHighScore
        highscorefont <- view $ cResources.cFont.highScoreFont
        drawTextToScreen highscorefont (T.pack . show $ highscore) (SDL.P (V2 0 0)) (SDL.V4 54 55 74 255) ((liftM (GuiTransforms.translate (V2 (0) (70)))) <$> (yCenterRectangle >=> xCenterRectangle))
        
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
        getPlayerIdleAnimation >>= replacePlayerAnimation 
        setPlayerYVel 0

updatePlayerAngle :: PlayerManager m => m ()
updatePlayerAngle = do
        yvel <- getPlayerYVel
        ang <- getPlayerAngle
        if yvel < 0 && ang >= (-10)
           then setPlayerAngle (ang - 1)
           else if ang <= 0
           then setPlayerAngle (ang + 1)
           else return ()
            
resetPlayerPos :: (PlayerManager m) => m ()
resetPlayerPos = setPlayerPos $ SDL.P $ V2 0 260

resetPlayerAngle :: PlayerManager m => m ()
resetPlayerAngle = setPlayerAngle 0

-- if the button is pressed, then execute the modifier to the game state
buttonGameStateModifierFromMouse :: (GameStateManager m 
  , HasInput m) => m ()  -- action to modify the game
  -> Button m
buttonGameStateModifierFromMouse f = do
        mousepos <- lift $ (\(V2 a b) -> (SDL.P (V2 (fromIntegral a) (fromIntegral b)))) <$> _mousePos <$> getInput
        mousepress <- lift $ _mousePress <$> getInput

        btnattr <- ask 

        if pointHitTest mousepos (aabb btnattr) && mousepress
           then lift f
           else return () 

getMouseAttrib :: HasInput m => m (Bool, V2 Float)
getMouseAttrib = do
        mousepos <- (\(V2 a b) -> V2 (fromIntegral a) (fromIntegral b)) <$> _mousePos <$> getInput
        mousepress <- _mousePress <$> getInput
        return (mousepress, mousepos)

updateCameraPos :: (PlayerManager m, CameraManager m) => m ()
updateCameraPos = do
        SDL.P (V2 x _) <- getPlayerPos
        camoffset <- getCameraOffset
        setCameraPos . SDL.P $ (V2 x 0) + camoffset
