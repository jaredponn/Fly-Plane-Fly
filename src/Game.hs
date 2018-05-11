{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE FlexibleContexts #-} 
{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE ScopedTypeVariables #-} 
{-# LANGUAGE FlexibleInstances #-} 

module Game (MahppyBird (..)
            , Config (..)
            , Vars (..)
            , loop
            , runMahppyBird
            )
                    where

import qualified SDL
import SDL (V2 (..))

import Data.StateVar (($=)
                     , get)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.C.Types
import Debug.Trace
import Control.Monad (unless)
import Linear.V2

import Logger
import Input
import Renderer
import Gravity
import Movement

-- ReaderT Environment Monad ReturnedVal
newtype MahppyBird a = MahppyBird (ReaderT Config (StateT Vars IO) a) 
        deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO)

-- http://lazyfoo.net/tutorials/SDL/30_scrolling/index.php
-- https://hackage.haskell.org/package/sdl2-2.4.0.1/docs/SDL-Raw-Types.html
data Config = Config { cWindow :: SDL.Window
                     , cRenderer :: SDL.Renderer
                     , cResources :: Resources

                     , cGrav :: Float
                     , cJumpHeight :: Float
                     , cCamOffset :: Float
                     , cRightVel :: Float}

data Resources = Resources

data Vars = Vars { playerPos :: V2 Float
                 , vel :: Float
                 , dt :: Time
                 , camera :: V2 CInt
                 , kInput :: Input }

data WallStream = WallStream

instance Show Vars where
        show vars = "Playerpos: " ++ show (playerPos vars) ++ "\n"
                ++ "Velocity: " ++ show (vel vars) ++ "\n"
                ++ "dt: " ++ show (dt vars) ++ "\n"
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
        , HasMovement m) 
        => m ()
loop = do
        input <- acquireInput
        renderScreen
        updatePhysics input
        fuck <- Control.Monad.State.get
        logToFile "/home/jared/Programs/mahppybird/log.txt" . show $ fuck
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
           then asks cJumpHeight >>= addVel
           else return ()
        applyVel
        -- moving character right
        rightVel <- asks cRightVel
        translate $ V2 rightVel 0


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
                presentwr
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
                pPos <- gets playerPos  >>= toScreenCord
                SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P pPos) (SDL.V2 20 20)

        drawWall :: (Logger m, Renderer m, MonadIO m, MonadReader Config m, MonadState Vars m) => m ()
        drawWall = undefined

        presentwr :: (MonadReader Config m, MonadIO m) => m ()
        presentwr = asks cRenderer >>= SDL.present

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
