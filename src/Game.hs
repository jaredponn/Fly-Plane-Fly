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
import PlayerManager

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
                     , cWallConf :: {-# UNPACK #-} !WallConfig
                     , cPlayerSize :: {-# UNPACK #-} !(V2 Float) }

data Resources = Resources

data Vars = Vars { playerPos :: {-# UNPACK #-} !(V2 Float)
                 , vel :: {-# UNPACK #-} !Float
                 , score ::{-# UNPACK #-} !Int
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
        , WallManager m
        , PlayerManager m) 
        => m ()
loop = do
        input <- acquireInput
        renderScreen
        updatePhysics input

        collisionTest
        updateWalls
        updateScore

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

updateWalls :: (PlayerManager m,  MonadReader Config m, WallManager m) => m ()
updateWalls = do
        V2 x _ <- getPlayerPos 
        fstWall <- getFirstWall
        wallwidth <- wallWidth <$> getFirstWall
        camoffset <- asks cCamOffset
        if xPos fstWall <= x - (wallwidth + abs camoffset)
           then popWall
           else return ()

collisionTest :: (WallManager m, PlayerManager m, Logger m) => m ()
collisionTest = do
        playerAabb <- getPlayerAabb
        entireWallAabb <- getEntireFirstWallAabb
        gapAabb <- getFirstWallGapAabb
        if (hitTestX playerAabb entireWallAabb) && (not (hitTest playerAabb gapAabb))
           then logText "you lose"
           else return ()

updateScore :: (Logger m, PlayerManager m, WallManager m, MonadState Vars m) => m ()
updateScore = do
        V2 xPlayer _ <- getPlayerPos 
        fstWall <- getFirstWall
        wallwidth <- wallWidth <$> getFirstWall
        currscore <- gets score
        let centerOfWallX = xPos fstWall + wallwidth / 2
        if (abs (xPlayer - centerOfWallX)) <= 0.15
           then do modify (\v -> v {score = currscore + 1})
                   gets score >>= logText . show 
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

        drawPlayer :: (Renderer m, MonadIO m, MonadReader Config m, PlayerManager m) => m ()
        drawPlayer = do
                renderer <- asks cRenderer 
                SDL.rendererDrawColor renderer $= SDL.V4 255 0 0 255
                (pPos, pSize) <- getPlayerAttributes
                pPos' <- toScreenCord pPos
                SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P pPos') (SDL.V2 20 20)

        drawWalls :: (Renderer m, WallManager m, MonadIO m, MonadReader Config m) => m ()
        drawWalls = do
                renderer <- asks cRenderer 
                SDL.rendererDrawColor renderer $= SDL.V4 0 255 0 255
                walls <- getWalls >>= mapM wallToSDLRect
                mapM_ (mapM_ (SDL.fillRect renderer . Just)) walls

        -- Mainly used for debugging
        drawRect :: (Renderer m, MonadIO m, MonadReader Config m, PlayerManager m) => (V2 Float, V2 Float) -> m ()
        drawRect (pos, transform) = do
                renderer <- asks cRenderer 
                SDL.rendererDrawColor renderer $= SDL.V4 0 0 255 255
                pos' <- toScreenCord pos
                let transform' = (\(V2 a b) -> (V2 (round a) (round b))) transform
                SDL.fillRect renderer $ Just $ SDL.Rectangle (SDL.P pos') transform'
                presentRenderer

        wallToSDLRect :: (Renderer m, WallManager m) => Wall -> m ([SDL.Rectangle CInt])
        wallToSDLRect wall = do
                ((topPoint, topLengths), (botPoint, botLengths)) <- transformToWorldCoord wall
                topPoint' <- toScreenCord topPoint
                botPoint' <- toScreenCord botPoint
                let (topLengths' :: V2 CInt) =  (\(V2 a b) -> (V2 (round a) (round b))) topLengths
                    (botLengths' :: V2 CInt) =  (\(V2 a b) -> (V2 (round a) (round b))) botLengths
                return $ [ SDL.Rectangle (SDL.P topPoint') topLengths'
                         , SDL.Rectangle (SDL.P botPoint') botLengths' ]

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
        transformToWorldCoord :: (MonadReader Config m) => Wall -> m ( (V2 Float, V2 Float) 
                                                                     , (V2 Float, V2 Float) )
        transformToWorldCoord wall = do
                (_, winH) <- (\(a,b) -> (fromIntegral a, fromIntegral b)) <$> asks cWindowSize
                let xPosition = V2 (xPos wall) 0
                    topPoint = xPosition
                    topHeight = winH * upperWall wall 
                    botPoint = xPosition + (V2 0 ((upperWall wall + gap wall) * winH))
                    botHeight = winH * lowerWall wall 
                    wallwidth = wallWidth wall
                return ( (topPoint, V2 wallwidth topHeight)
                       , (botPoint, V2 wallwidth botHeight))


        getWalls :: (MonadReader Config m, MonadState Vars m) => m ([Wall])
        getWalls = do
                (winW, _) <- (\(a,b) -> (fromIntegral a, fromIntegral b)) <$> asks cWindowSize 
                wallConf <- asks cWallConf
                wallstream <- gets wallStream
                return $ Stream.take (ceiling (winW / (allWallWidth wallConf + allWallSpacing wallConf))) wallstream

        getFirstWall :: (MonadState Vars m) => m (Wall)
        getFirstWall = Stream.head <$> gets wallStream 

        getEntireFirstWallAabb :: (WallManager m) => m Aabb
        getEntireFirstWallAabb = do
                ((V2 topX topY, _), (V2 botX botY, V2 botwidth botheight)) <- getFirstWall >>= transformToWorldCoord
                return $ Aabb (V2 topX topY) (V2 (botX + botwidth) (botY + botheight))

        getFirstWallGapAabb :: (WallManager m, Renderer m) => m (Aabb)
        getFirstWallGapAabb = do 
                ((V2 topX _, V2 _ topheight), (V2 botX botY, V2 botwidth _))  <- getFirstWall >>= transformToWorldCoord
                return $ Aabb (V2 topX topheight) (V2 (botwidth + botX) botY )


        popWall :: MonadState Vars m => m ()
        popWall = do
                wallstream <- gets wallStream
                modify (\v -> v { wallStream = Stream.tail wallstream } )

instance PlayerManager MahppyBird where
        getPlayerPos ::(MonadState Vars m) => m (V2 Float)
        getPlayerPos = gets playerPos

        getPlayerAttributes ::(MonadState Vars m, MonadReader Config m) => m (V2 Float, V2 Float)
        getPlayerAttributes = do
                ppos <- gets playerPos
                psize <- asks cPlayerSize
                return (ppos, psize)

        getPlayerAabb :: (PlayerManager m) => m (Aabb)
        getPlayerAabb = do
                (V2 x y, V2 wlength hlength) <- getPlayerAttributes
                return $ Aabb (V2 x y) (V2 (x + wlength) (y + hlength))
