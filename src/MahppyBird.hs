{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE BangPatterns #-} 
{-# LANGUAGE FlexibleInstances #-} 

module MahppyBird (MahppyBird (..)
            , Config (..)
            , Vars (..)
            , loop
            , runMahppyBird
            ) where

import qualified SDL
import SDL (V2 (..))
import qualified SDL.Font as Font

import Data.StateVar (($=))
import Control.Monad.Reader
import Control.Monad.State
import Foreign.C.Types
import Control.Monad (unless)
import Linear.V2
import Data.Stack
import qualified Data.Stream as Stream
import Data.Stream (Stream (..))
import qualified System.Clock

import GameVars

import Aabb
import Walls
import Logger
import Input
import Renderer
import Physics
import Walls
import ButtonTransforms
import WallManager
import PlayerManager
import ScoreManager
import TimeManager
import GameStateManager
import CameraManager

import GameVars



runMahppyBird :: Config -> Vars -> MahppyBird a -> IO a 
runMahppyBird conf vars (MahppyBird m) = evalStateT (runReaderT m conf) vars

acquireInput :: (Logger m, HasInput m) => m Input
acquireInput = do
        updateInput
        getInput

loop :: ( Logger m
        , Renderer m
        , HasInput m
        , Physics m
        , WallManager m
        , PlayerManager m
        , ScoreManager m
        , CameraManager m
        , MonadState Vars m
        , TimeManager m
        , ButtonTransforms m 
        , GameStateManager m ) => m ()
loop = do
        input <- acquireInput
        curgamestate <- peekGameState

        runScene input curgamestate

        playState <- get
        logToFile "/home/jared/Programs/mahppybird/log.txt" . show $ playState

        unless (curgamestate == Quit) loop 

runScene :: ( Logger m
            , Renderer m
            , HasInput m
            , Physics m
            , WallManager m
            , PlayerManager m
            , ScoreManager m
            , CameraManager m
            , TimeManager m
            , ButtonTransforms m 
            , GameStateManager m ) => Input -> GameState -> m ()
runScene input Menu = do
        mousepos <- (\(V2 a b) -> V2 (fromIntegral a) (fromIntegral b)) <$> mousePos <$> getInput
        mousepress <- mousePress <$> getInput

        playbtnattr <- createXCenteredButtonAttr 100 (V2 600 200)
        runReaderT playbtneffect playbtnattr

        quitbtnattr <- createXCenteredButtonAttr 500 (V2 600 100)
        runReaderT quitbtneffect quitbtnattr

        drawObjects [drawBg, drawRectToScreen (rect playbtnattr), drawRectToScreen (rect quitbtnattr)]

        where
                playbtneffect ::( GameStateManager m
                  , HasInput m) => Button m
                playbtneffect = buttonGameStateModifierFromMouse . pushGameState $ Play

                quitbtneffect ::( GameStateManager m
                                , HasInput m) => Button m
                quitbtneffect = buttonGameStateModifierFromMouse . pushGameState $ Quit



runScene input Play = do
        renderScreen
        updatePhysics input
        shouldPause input

        collisionTest
        updateWalls
        updateScore
        where

                updatePhysics :: (Physics m, PlayerManager m, TimeManager m) => Input -> m ()
                updatePhysics input = do
                        -- applying gravity
                        applyGrav
                        if isSpace input
                           then jumpPlayer
                           else return ()
                        applyYVel
                        -- moving character right
                        applyXVel

                shouldPause :: (Physics m, GameStateManager m) => Input -> m ()
                shouldPause input = do
                        if isEsc input
                           then pushGameState Pause
                           else return ()

                updateWalls :: (PlayerManager m, WallManager m, CameraManager m) => m ()
                updateWalls = do
                        V2 x _ <- getPlayerPos 
                        fstWall <- getFirstWall
                        wallwidth <- wallWidth <$> getFirstWall
                        V2 xcamoffset _ <- getCameraOffset
                        if xPos fstWall <= x - (wallwidth + abs xcamoffset)
                           then popWall_
                           else return ()

                updateScore :: (Logger m, PlayerManager m, WallManager m, ScoreManager m) => m ()
                updateScore = do
                        V2 xPlayer _ <- getPlayerPos 
                        fstWall <- getFirstWall
                        wallwidth <- wallWidth <$> getFirstWall
                        currscore <- getScore
                        let centerOfWallX = xPos fstWall + wallwidth / 2
                        if (abs (xPlayer - centerOfWallX)) <= 0.15
                           then do incrementScore
                                   getScore >>= logText . show
                           else return ()

                renderScreen :: (Logger m, Renderer m, PlayerManager m, CameraManager m) => m ()
                renderScreen = do
                        renderGame []

                collisionTest :: (WallManager m, PlayerManager m, Logger m, ScoreManager m, GameStateManager m) => m ()
                collisionTest = do
                        playerAabb <- getPlayerAabb
                        upperWallAabb <- floorAabb <$> getFirstUpperWallAabb
                        lowerWallAabb <- ceilingAabb <$> getFirstLowerWallAabb
                        if (hitTestAbove playerAabb upperWallAabb) || (hitTestBelow playerAabb lowerWallAabb)
                        then do 
                                logText "you lose. FINAL SCORE: "
                                getScore >>= logText . show 
                                pushGameState GameOver
                        else return ()

runScene input Pause = do
        renderScreen
        if isEsc input || isSpace input
           then popGameState_
           else return ()
        where
                renderScreen = renderGame []

runScene input GameOver = do

        playagainbtnattr <- createXCenteredButtonAttr 100 (V2 600 200)
        runReaderT playagainbtneffect playagainbtnattr

        quitbtnattr <- createXCenteredButtonAttr 500 (V2 600 100)
        runReaderT quitbtneffect quitbtnattr

        renderScreen playagainbtnattr quitbtnattr

        updatePhysics 
        where
                renderScreen :: (Logger m, Renderer m, PlayerManager m, CameraManager m) => ButtonAttr
                             -> ButtonAttr
                             -> m ()
                renderScreen btn0 btn1= do
                        renderGame [drawRectToScreen (rect btn0), drawRectToScreen (rect btn1)]

                updatePhysics :: (Physics m) => m ()
                updatePhysics = do
                        applyGrav
                        applyYVel

                playagainbtneffect ::( GameStateManager m
                                     , HasInput m
                                     , WallManager m
                                     , PlayerManager m
                                     , ScoreManager m) => Button m
                playagainbtneffect = buttonGameStateModifierFromMouse $ popGameState_ >> pushGameState Play >> resetGame

                quitbtneffect ::( GameStateManager m
                                , HasInput m) => Button m
                quitbtneffect = buttonGameStateModifierFromMouse . pushGameState $ Quit

runScene input Quit = return ()


renderGame :: (Logger m, Renderer m, PlayerManager m, CameraManager m) => [m ()] ->m ()
renderGame renderactions = do
        V2 x _ <- getPlayerPos
        camoffset <- getCameraOffset
        setCameraPos $ (V2 x 0) + camoffset
        drawObjectsWithDt $ [drawBg, drawWalls, drawPlayer] ++ renderactions

resetGame :: (WallManager m, PlayerManager m, ScoreManager m) => m ()
resetGame = do
        resetScore
        resetWalls
        resetPlayerPos
        setPlayerYVel 0

-- if the button is pressed, then execute the modifier to the game state
buttonGameStateModifierFromMouse :: (GameStateManager m 
  , HasInput m) => m ()  -- action to modify the game
  -> Button m
buttonGameStateModifierFromMouse f = do
        mousepos <- lift $ (\(V2 a b) -> V2 (fromIntegral a) (fromIntegral b)) <$> mousePos <$> getInput
        mousepress <- lift $ mousePress <$> getInput

        btnattr <- ask 

        if pointHitTest mousepos (aabb btnattr) && mousepress
           then lift $ f
           else return () 
