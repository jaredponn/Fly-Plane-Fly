{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE BangPatterns #-} 
{-# LANGUAGE FlexibleInstances #-} 

module Game (MahppyBird (..)
            , Config (..)
            , Vars (..)
            , loop
            , runMahppyBird
            ) where

import qualified SDL
import SDL (V2 (..))

import Data.StateVar (($=))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.Types
import Debug.Trace
import Control.Monad (unless)
import Linear.V2
import Data.List (intercalate)
import qualified Data.Stream as Stream
import Data.Stream (Stream (..))
import Control.Concurrent (threadDelay)

import Aabb
import Walls
import Logger
import Input
import Renderer
import Gravity
import Movement
import Walls
import WallManager

-- ReaderT Environment Monad ReturnedVal
newtype MahppyBird a = MahppyBird (ReaderT Config (StateT Vars IO) a) 
        deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO)

-- http://lazyfoo.net/tutorials/SDL/30_scrolling/index.php
-- https://hackage.haskell.org/package/sdl2-2.4.0.1/docs/SDL-Raw-Types.html
data Config = Config { cWindow :: SDL.Window
                     , cRenderer :: SDL.Renderer
                     , cResources :: Resources
                     , cWindowSize :: {-# UNPACK #-} !(CInt,CInt) --width, height

                     , cGrav ::{-# UNPACK #-} !Float
                     , cJumpHeight :: {-# UNPACK #-} !Float
                     , cCamOffset :: {-# UNPACK #-} !Float
                     , cRightVel :: {-# UNPACK #-} !Float
                     , cWallConf :: {-# UNPACK #-} !WallConfig}

data Resources = Resources

data Vars = Vars { playerPos :: {-# UNPACK #-} !(V2 Float)
                 , vel :: {-# UNPACK #-} !Float
                 , dt :: {-# UNPACK #-} !Time
                 , camera :: {-# UNPACK #-} !(V2 CInt)
                 , kInput :: Input 
                 , wallStream :: Stream Wall }

instance Show Vars where
        show vars = "Playerpos: " ++ show (playerPos vars) ++ "\n"
                ++ "Velocity: " ++ show (vel vars) ++ "\n"
                ++ "dt: " ++ show (dt vars) ++ "\n"
                ++ "FPS: " ++ show (10000.001 / (dt vars)) ++ "\n"
                ++ "camera: " ++ show (camera vars) ++ "\n"
                ++ "kInput: " ++ show (kInput vars) ++ "\n"
                


runMahppyBird :: Config -> Vars -> MahppyBird a -> IO a 
runMahppyBird conf vars (MahppyBird m) = evalStateT (runReaderT m conf) vars

loop :: (MonadReader Config m
        , MonadState Vars m
        , MonadIO m
        , Logger m
        , Renderer m
        , HasInput m
        , HasGravity m
        , HasMovement m
        , WallManager m) 
        => m ()
loop = do
        input <- acquireInput
        renderScreen
        updatePhysics input
        -- collisions
        updateWalls
        gameState <- get
        logToFile "/home/jared/Programs/mahppybird/log.txt" . show $ gameState
        unless (isEsc input) loop 

acquireInput :: (Logger m, HasInput m, MonadState Vars m) => m Input
acquireInput = do
        updateInput
        getInput

renderScreen :: (Logger m, Renderer m, MonadIO m, MonadState Vars m, MonadReader Config m) => m ()
renderScreen = do
        drawScreen
        V2 x _ <- gets playerPos
        camOffset <- asks cCamOffset
        setCameraPos $ V2 (x + camOffset) 0

updatePhysics :: (Logger m, HasGravity m, HasMovement m, MonadState Vars m, MonadReader Config m) => Input -> m ()
updatePhysics input = do
        -- applying gravity
        applyGrav
        if isSpace input
           then asks cJumpHeight >>= setVel
           else return ()
        applyVel
        -- moving character right
        rightVel <- asks cRightVel
        translate $ V2 rightVel 0

updateWalls :: (MonadState Vars m, MonadReader Config m, WallManager m) => m ()
updateWalls = do
        V2 x _ <- gets playerPos
        fstWall <- Stream.head <$> gets wallStream
        if xPos fstWall < x - 400
           then popWall
           else return ()

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
                let space = any (eventIs SDL.KeycodeSpace) events
                    esc = any (eventIs SDL.KeycodeEscape) events
                setInput $ Input {isSpace = space
                                 , isEsc = esc}

        setInput :: (MonadState Vars m) => Input -> m ()
        setInput input = modify (\v -> v { kInput = input })

        getInput :: (MonadState Vars m) => m Input
        getInput = gets kInput

instance Renderer MahppyBird where
        -- http://headerphile.com/sdl2/sdl2-part-3-drawing-rectangles/
        drawScreen :: (Renderer m, MonadIO m, MonadState Vars m) => m ()
        drawScreen = do
                t0 <- SDL.time
                drawBg
                drawPlayer
                drawWalls
                presentRenderer
                liftIO $ threadDelay 2000 -- fixes the weird speed ups sometimes
                t1 <- SDL.time
                modify (\v -> v { dt = (t1 - t0) * 1000 } )
                return ()

        drawBg :: (Renderer m, MonadIO m, MonadReader Config m, MonadState Vars m) => m ()
        drawBg = do
                renderer <- asks cRenderer 
                SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255
                SDL.clear renderer

        drawPlayer :: (Logger m, Renderer m, MonadIO m, MonadReader Config m, MonadState Vars m) => m ()
        drawPlayer = do
                renderer <- asks cRenderer 
                SDL.rendererDrawColor renderer $= SDL.V4 255 0 0 255
                pPos <- gets playerPos >>= toScreenCord
                SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P pPos) (SDL.V2 20 20)

        -- TODO
        drawWalls :: (Logger m, Renderer m, WallManager m, MonadIO m, MonadReader Config m) => m ()
        drawWalls = do
                renderer <- asks cRenderer 
                SDL.rendererDrawColor renderer $= SDL.V4 0 255 0 255
                walls <- getWalls >>= mapM wallToSDLRect
                mapM_ (mapM_ (SDL.fillRect renderer . Just)) walls


        -- TODO
        wallToSDLRect :: (Renderer m, MonadReader Config m, MonadState Vars m) => Wall -> m ([SDL.Rectangle CInt])
        wallToSDLRect wall = do
                (_, winH) <- (\(a,b) -> (fromIntegral a, fromIntegral b)) <$> asks cWindowSize
                xPosScreen <- toScreenCord $ V2 (xPos wall) (0 :: Float)
                let topPoint = xPosScreen
                    topHeight = round $ winH * upperWall wall 
                    botPoint = xPosScreen + (V2 0 (round ((upperWall wall + gap wall) * winH)))
                    botHeight = round $ winH * lowerWall wall 
                    wallwidth = round $ wallWidth wall
                return $ [ SDL.Rectangle (SDL.P topPoint) (SDL.V2 wallwidth topHeight)
                         , SDL.Rectangle (SDL.P botPoint) (SDL.V2 wallwidth botHeight) ]

        presentRenderer :: (MonadReader Config m, MonadIO m) => m ()
        presentRenderer = asks cRenderer >>= SDL.present

        toScreenCord :: (MonadState Vars m) => V2 Float -> m (V2 CInt)
        toScreenCord wPos = do
                let pos = roundCoord wPos
                (cam :: V2 CInt) <- gets camera 
                return $ pos - cam

        moveCameraBy :: (MonadState Vars m) => V2 CInt -> m ()
        moveCameraBy transform = do
                (cam :: V2 CInt) <- gets camera 
                modify (\v -> v { camera  = cam + transform } )

        setCameraPos :: (MonadState Vars m) => V2 Float -> m ()
        setCameraPos cam = do
                modify (\v -> v { camera  = roundCoord cam } )

instance HasGravity MahppyBird where
        -- converts the gravity into velocity 
        applyGrav :: (MonadReader Config m, MonadState Vars m) => m  ()
        applyGrav = do 
                acc <- asks cGrav
                ndt <-  gets dt
                curVel <- gets vel
                modify (\v -> v { vel = curVel + ndt * acc } )

        -- allows the addition of any given velocity number to the current velocity
        addVel :: (MonadState Vars m) => Float -> m  ()
        addVel dv = do 
                curVel <- gets vel
                modify (\v -> v { vel = curVel + dv} )

        -- sets the current belocity to a new one
        setVel :: (MonadState Vars m) => Float -> m  ()
        setVel nv = do 
                modify (\v -> v { vel = nv} )
        
        -- applies the velocity to the position by using dt to update the y position
        applyVel :: (HasMovement m, MonadState Vars m) => m ()
        applyVel = do
                curVel <- gets vel
                curdt <-  gets dt
                translate $ V2 0 (curVel * curdt)
        
instance HasMovement MahppyBird where
        translate :: (MonadState Vars m) => V2 Float -> m ()
        translate n = do
                curPos <- gets playerPos
                modify (\v -> v {playerPos = curPos + n})

instance WallManager MahppyBird where
        transformToWorldCoord :: (MonadReader Config m) => Wall -> m ( (V2 Float, (Float, Float))
                                                                     , (V2 Float, (Float, Float)))
        transformToWorldCoord wall = do
                (_, winH) <- (\(a,b) -> (fromIntegral a, fromIntegral b)) <$> asks cWindowSize
                let xPosition = V2 (xPos wall) 0
                    topPoint = xPosition
                    topHeight = winH * upperWall wall 
                    botPoint = xPosition + (V2 0 ((upperWall wall + gap wall) * winH))
                    botHeight = winH * lowerWall wall 
                    wallwidth = wallWidth wall
                return ( (topPoint, (wallwidth, topHeight))
                       , (botPoint, (wallwidth, botHeight)))


        getWalls :: (MonadReader Config m, MonadState Vars m) => m ([Wall])
        getWalls = do
                (winW, _) <- (\(a,b) -> (fromIntegral a, fromIntegral b)) <$> asks cWindowSize 
                wallConf <- asks cWallConf
                wallstream <- gets wallStream
                return $ Stream.take (ceiling (winW / (allWallWidth wallConf + allWallSpacing wallConf))) wallstream

        getFirstWall :: (MonadState Vars m) => m (Wall)
        getFirstWall = Stream.head <$> gets wallStream 

        getFirstWallAabb :: (MonadState Vars m) => m (Aabb)
        getFirstWallAabb = undefined

        popWall :: MonadState Vars m => m ()
        popWall = do
                wallstream <- gets wallStream
                modify (\v -> v { wallStream = Stream.tail wallstream } )

