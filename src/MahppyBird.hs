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
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.Types
import Control.Monad (unless)
import Linear.V2
import Data.Stack
import qualified Data.Stream as Stream
import Data.Stream (Stream (..))
import qualified Control.Concurrent (threadDelay)
import qualified System.Clock

import GameVars

import Aabb
import Walls
import Logger
import Input
import Renderer
import Gravity
import Movement
import Walls
import WallManager
import PlayerManager
import ScoreManager
import TimeManager
import GameStateManager
import CameraManager

import Config

-- ReaderT Environment Monad ReturnedVal
newtype MahppyBird a = MahppyBird (ReaderT Config (StateT Vars IO) a) 
        deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO)

-- http://lazyfoo.net/tutorials/SDL/30_scrolling/index.php
-- https://hackage.haskell.org/package/sdl2-2.4.0.1/docs/SDL-Raw-Types.html


instance Show Vars where
        show vars = 
                {-    "Playerpos: " ++ show (playerPos vars) ++ "\n" -}
                {- ++ "Velocity: " ++ show (vel vars) ++ "\n" -}
                "rip"
                ++ "dt: " ++ show (dt vars) ++ "\n"
                ++ "FPS: " ++ show (10000.001 / (dt vars)) ++ "\n"
                ++ "camera: " ++ show (camera vars) ++ "\n"
                ++ "kInput: " ++ show (kInput vars) ++ "\n"
                


runMahppyBird :: Config -> Vars -> MahppyBird a -> IO a 
runMahppyBird conf vars (MahppyBird m) = evalStateT (runReaderT m conf) vars

acquireInput :: (Logger m, HasInput m, MonadState Vars m) => m Input
acquireInput = do
        updateInput
        getInput

loop :: ( MonadReader Config m
        , MonadState Vars m
        , MonadIO m
        , Logger m
        , Renderer m
        , HasInput m
        , HasGravity m
        , HasMovement m
        , WallManager m
        , PlayerManager m
        , ScoreManager m
        , CameraManager m
        , GameStateManager m ) => m ()
loop = do
        input <- acquireInput
        curgamestate <- peekGameState

        runScene input curgamestate

        playState <- get
        logToFile "/home/jared/Programs/mahppybird/log.txt" . show $ playState

        unless (curgamestate == Quit) loop 


runScene :: ( MonadReader Config m
            , MonadState Vars m
            , MonadIO m
            , Logger m
            , Renderer m
            , HasInput m
            , HasGravity m
            , HasMovement m
            , WallManager m
            , PlayerManager m
            , ScoreManager m
            , CameraManager m
            , GameStateManager m ) => Input -> GameState -> m ()
runScene input Menu = do
        mousepos <- (\(V2 a b) -> V2 (fromIntegral a) (fromIntegral b)) <$> mousePos <$> getInput
        mousepress <- mousePress <$> getInput

        let playbtn = Aabb (V2 100 100) (V2 200 200)
        let quitbtn = Aabb (V2 300 400) (V2 400 500)
        drawObjects [drawBg, drawRectToScreen (aabbToDrawRect playbtn), drawRectToScreen (aabbToDrawRect quitbtn)]

        if pointHitTest mousepos playbtn && mousepress
           then pushGameState Play
           else return () 

        if pointHitTest mousepos quitbtn && mousepress
           then pushGameState Quit
           else return ()

runScene input Play = do
        renderScreen
        updatePhysics input
        shouldPause input

        collisionTest
        updateWalls
        updateScore
        where

                updatePhysics :: (HasGravity m, HasMovement m, MonadState Vars m) => Input -> m ()
                updatePhysics input = do
                        -- applying gravity
                        applyGrav
                        if isSpace input
                           then gets cJumpHeight >>= setVel
                           else return ()
                        applyVel
                        -- moving character right
                        rightVel <- gets cRightVel
                        translate $ V2 rightVel 0

                shouldPause :: (HasGravity m, HasMovement m, MonadState Vars m, GameStateManager m) => Input -> m ()
                shouldPause input = do
                        if isEsc input
                           then pushGameState Pause
                           else return ()

                updateWalls :: (PlayerManager m,  MonadState Vars m, WallManager m) => m ()
                updateWalls = do
                        V2 x _ <- getPlayerPos 
                        fstWall <- getFirstWall
                        wallwidth <- wallWidth <$> getFirstWall
                        V2 xcamoffset _ <- gets cCamOffset
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

                renderScreen :: (Logger m, Renderer m, MonadIO m, PlayerManager m, CameraManager m) => m ()
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
        mousepos <- (\(V2 a b) -> V2 (fromIntegral a) (fromIntegral b)) <$> mousePos <$> getInput
        mousepress <- mousePress <$> getInput

        updatePhysics 

        let playagainbtn = Aabb (V2 100 100) (V2 200 200)
        let quitbtn = Aabb (V2 500 600) (V2 700 700)

        renderScreen playagainbtn quitbtn

        if pointHitTest mousepos playagainbtn && mousepress
           then do popGameState_
                   pushGameState Play
                   resetGame
           else return () 

        if pointHitTest mousepos quitbtn && mousepress
           then pushGameState Quit
           else return ()


        where
                renderScreen :: (Logger m, Renderer m, MonadIO m, PlayerManager m, CameraManager m) => Aabb
                             -> Aabb
                             -> m ()
                renderScreen btn0 btn1= do
                        renderGame $ map (drawRectToScreen . aabbToDrawRect) [btn0, btn1]

                updatePhysics :: (HasGravity m, HasMovement m, MonadState Vars m) => m ()
                updatePhysics = do
                        applyGrav
                        applyVel

runScene input Quit = return ()


renderGame :: (Logger m, Renderer m, MonadIO m, PlayerManager m, CameraManager m) => [m ()] ->m ()
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
        setPlayerVel 0




instance Logger MahppyBird where
        logText :: (MonadIO m) => String -> m ()
        logText str = do
                t <- SDL.ticks
                liftIO . putStrLn $ (show t) ++ ": " ++ str ++ "\n"

        logToFile :: (MonadIO m) => FilePath -> String -> m ()
        logToFile path str = do
                t <- SDL.ticks
                liftIO $ appendFile path ((show t) ++ ": " ++ str ++ "\n")

instance HasInput MahppyBird where
        updateInput :: (HasInput m, MonadIO m, MonadState Vars m) => m ()
        updateInput = do
                events <- SDL.pollEvents
                SDL.P mousepos <- liftIO  SDL.getAbsoluteMouseLocation
                mousepress <- liftIO SDL.getMouseButtons
                let space = any (eventIs SDL.KeycodeSpace) events
                    esc = any (eventIs SDL.KeycodeEscape) events

                setInput Input { isSpace = space
                               , isEsc = esc 
                               , mousePos = mousepos
                               , mousePress = foldr ((||) . mousepress) False [SDL.ButtonLeft, SDL.ButtonMiddle, SDL.ButtonRight] }

        setInput :: (MonadState Vars m) => Input -> m ()
        setInput input = modify (\v -> v { kInput = input })

        getInput :: (MonadState Vars m) => m Input
        getInput = gets kInput

instance Renderer MahppyBird where
        drawObjects :: (Renderer m, TimeManager m) => [m ()] -> m ()
        drawObjects drawactions = do
                mapM_ id drawactions
                presentRenderer

        drawObjectsWithDt :: (Renderer m, TimeManager m) => [m ()] -> m ()
        drawObjectsWithDt drawactions = do
                t0 <- getRealTime 
                drawObjects drawactions
                threadDelay 2000 -- fixes the weird speed ups sometimes
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
                SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P pos') lengths'

        presentRenderer :: (MonadReader Config m, MonadIO m) => m ()
        presentRenderer = asks cRenderer >>= SDL.present

        toScreenCord :: (CameraManager m) => V2 Float -> m (V2 CInt)
        toScreenCord wPos = do
                let pos = roundV2 wPos
                (cam :: V2 CInt) <- getCamera
                return $ pos - cam


instance HasGravity MahppyBird where
        -- converts the gravity into velocity 
        applyGrav :: (MonadState Vars m, PlayerManager m, TimeManager m) => m  ()
        applyGrav = do 
                acc <- gets cGrav
                ndt <- getdt
                curvel <- getPlayerVel
                setPlayerVel $ curvel + ndt * acc

        -- allows the addition of any given velocity number to the current velocity
        addVel :: PlayerManager m => Float -> m  ()
        addVel dv = do 
                curvel <- getPlayerVel
                setPlayerVel $ curvel + dv

        -- sets the current velocity to a new one
        setVel :: PlayerManager m => Float -> m  ()
        setVel nv = do 
                setPlayerVel nv
        
        -- applies the velocity to the position by using dt to update the y position
        applyVel :: (HasMovement m, MonadState Vars m, PlayerManager m, TimeManager m) => m ()
        applyVel = do
                curvel <- getPlayerVel
                curdt <-  getdt
                translate $ V2 0 (curvel * curdt)
        
instance HasMovement MahppyBird where
        translate :: PlayerManager m => V2 Float -> m ()
        translate transform = do
                curpos <- getPlayerPos
                setPlayerPos $ transform + curpos

instance WallManager MahppyBird where
        transformWallLengthsToWorldVals :: (MonadReader Config m) => Wall -> m Wall
        transformWallLengthsToWorldVals wall = do
                (_, winH) <- (\(a,b) -> (fromIntegral a, fromIntegral b)) <$> asks cWindowSize
                return Wall { upperWall = winH * upperWall wall
                            , gap = winH * gap wall
                            , lowerWall = winH * lowerWall wall
                            , xPos = xPos wall
                            , wallWidth = wallWidth wall }

        getWallsInScreen :: (MonadState Vars m, MonadReader Config m, WallManager m) => m ([Wall])
        getWallsInScreen = do
                (winW, _) <- (\(a,b) -> (fromIntegral a, fromIntegral b)) <$> asks cWindowSize 
                wallconf <- gets cWallConf
                wallstream <- wallStream <$> gets vPlayVars
                let wallstorender = Stream.take (ceiling (winW / (allWallWidth wallconf + allWallSpacing wallconf))) wallstream
                mapM transformWallLengthsToWorldVals wallstorender

        getFirstWall :: (MonadState Vars m, WallManager m) => m (Wall)
        getFirstWall = Stream.head <$> wallStream <$> gets vPlayVars >>= transformWallLengthsToWorldVals

        getFirstUpperWallAabb :: WallManager m => m Aabb
        getFirstUpperWallAabb = do
                fstwall <- getFirstWall
                return $ Aabb (V2 (xPos fstwall) 0) (V2 (wallWidth fstwall + xPos fstwall) (upperWall fstwall))

        getFirstLowerWallAabb :: WallManager m => m Aabb
        getFirstLowerWallAabb = do
                fstwall <- getFirstWall
                return $ Aabb (V2 (xPos fstwall) (gap fstwall + upperWall fstwall)) (V2 (wallWidth fstwall + xPos fstwall) (upperWall fstwall + gap fstwall + lowerWall fstwall))

        popWall :: (MonadState Vars m, WallManager m) => m ( Wall )
        popWall = do
                playvars <- gets vPlayVars
                fstwall <- getFirstWall
                wallstream <- wallStream <$> gets vPlayVars
                modify (\v -> v { vPlayVars = playvars { wallStream = Stream.tail wallstream } } )
                return fstwall

        popWall_ :: (MonadState Vars m, WallManager m) => m ()
        popWall_ = do
                playvars <- gets vPlayVars
                wallstream <- wallStream <$> gets vPlayVars
                modify (\v -> v { vPlayVars = playvars { wallStream = Stream.tail wallstream } } )

        resetWalls :: (MonadState Vars m, MonadIO m) => m ()
        resetWalls = do
                playvars <- gets vPlayVars
                wallconf <- gets cWallConf
                wallstream <- liftIO . createWallStream $ wallconf
                modify (\v -> v { vPlayVars = playvars { wallStream = wallstream } } )

instance PlayerManager MahppyBird where
        getPlayerPos ::(MonadState Vars m) => m (V2 Float)
        getPlayerPos = playerPos <$> gets vPlayVars

        getPlayerAttributes ::(MonadState Vars m, MonadReader Config m) => m (V2 Float, V2 Float)
        getPlayerAttributes = do
                ppos <- playerPos <$> gets vPlayVars
                psize <- gets cPlayerSize
                return (ppos, psize)

        getPlayerAabb :: (PlayerManager m) => m (Aabb)
        getPlayerAabb = do
                (V2 x y, V2 wlength hlength) <- getPlayerAttributes
                return $ Aabb (V2 x y) (V2 (x + wlength) (y + hlength))

        resetPlayerPos :: (MonadState Vars m) => m ()
        resetPlayerPos = do
                playvars <- gets vPlayVars
                modify (\v -> v { vPlayVars = playvars { playerPos = 0 }  })

        getPlayerVel :: (MonadState Vars m) => m (Float)
        getPlayerVel = vel <$> gets vPlayVars

        setPlayerPos :: MonadState Vars m => V2 Float -> m ()
        setPlayerPos npos = do
                playvars <- gets vPlayVars
                modify (\v -> v { vPlayVars = playvars { playerPos = npos }  })

        setPlayerVel :: MonadState Vars m => Float -> m ()
        setPlayerVel nvel = do
                playvars <- gets vPlayVars
                modify (\v -> v { vPlayVars = playvars { vel = nvel }  })

        

instance ScoreManager MahppyBird where
        getScore :: MonadState Vars m => m (Int)
        getScore = gets score

        incrementScore :: MonadState Vars m => m ()
        incrementScore = do
                currscore <- gets score
                modify (\v -> v { score = currscore + 1} )

        resetScore :: MonadState Vars m => m ()
        resetScore = modify (\v -> v { score = 0} )

instance TimeManager MahppyBird where
        getRealTime :: MonadIO m => m (System.Clock.TimeSpec)
        getRealTime = liftIO . System.Clock.getTime $ System.Clock.Realtime

        threadDelay :: MonadIO m => Int -> m ()
        threadDelay = liftIO . Control.Concurrent.threadDelay 

        setdt :: MonadState Vars m => Float -> m ()
        setdt ndt = modify (\v -> v { dt = ndt } )

        getdt :: MonadState Vars m => m (Float)
        getdt = gets dt

instance GameStateManager MahppyBird where
        pushGameState :: MonadState Vars m => GameState -> m ()
        pushGameState nstate = do
                gamestack <- gets vGameStateStack
                let nstack = stackPush gamestack nstate
                modify (\v -> v { vGameStateStack = nstack } )

        popGameState :: MonadState Vars m => m (GameState)
        popGameState = do
                gamestack <- gets vGameStateStack
                -- ADD ERROR HANDLING
                let Just (nstack, val) = stackPop gamestack
                modify (\v -> v { vGameStateStack = nstack } )
                return val

        popGameState_ :: MonadState Vars m => m ()
        popGameState_ = do
                gamestack <- gets vGameStateStack
                -- ADD ERROR HANDLING
                let Just (nstack, val) = stackPop gamestack
                modify (\v -> v { vGameStateStack = nstack } )

        peekGameState :: MonadState Vars m => m (GameState)
        peekGameState = do
                gamestack <- gets vGameStateStack
                let Just (nstack, val) = stackPop gamestack
                return val

instance CameraManager MahppyBird where
        moveCameraBy :: (MonadState Vars m) => V2 CInt -> m ()
        moveCameraBy transform = do
                (cam :: V2 CInt) <- gets camera 
                modify (\v -> v { camera  = cam + transform } )

        setCameraPos :: (MonadState Vars m) => V2 Float -> m ()
        setCameraPos cam = do
                modify (\v -> v { camera  = roundV2 cam } )


        getCamera :: (MonadState Vars m ) => m (V2 CInt)
        getCamera = gets camera

        getCameraOffset :: MonadState Vars m => m (V2 Float)
        getCameraOffset = gets cCamOffset

