{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import SDL (($=)
           , V2 (..))
import qualified SDL.Font as TTF
import Foreign.C.Types
import Control.Monad (unless)
import Data.Stack

import MahppyBird
import Config
import Walls
import GameVars

main :: IO ()
main = do
        SDL.initializeAll
        TTF.initialize

        window <- SDL.createWindow "Mahppy Bird" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
        renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

        font <- TTF.load fontPath 15

        let cfg = Config { cWindow = window
                         , cRenderer = renderer
                         , cWindowSize = (screenWidth, screenHeight)
                         , cResources = Resources { cFont = font } }


        wallstream <- createWallStream wallConf

        let pvars = PlayVars { playerPos  = V2 50 2
                               , vel = 0.0001
                               , wallStream = wallstream }
            vars = Vars { vGameStateStack = stackPush stackNew Menu
                        , vPlayVars = pvars
                        , dt = 0
                        , score = 0
                        , camera = V2 0 0
                        , kInput = undefined

                        , cGrav = 2900
                        , cJumpHeight = (-700)
                        , cRightVel = 0.3
                        , cCamOffset = (-100)
                        , cWallConf = wallConf
                        , cPlayerSize = V2 30 30 }

        runMahppyBird cfg vars loop

        TTF.quit
        SDL.quit

        return ()

screenWidth, screenHeight :: CInt
screenWidth = 1280
screenHeight = 720

wallConf :: WallConfig
wallConf =  WallConfig { allUppperWallRngBounds = (0.1, 0.58)
                           , allGapSize = 0.22
                           , allWallWidth = 100
                           , allWallSpacing = 175
                           , startingPos = 200 }

fontPath :: FilePath
fontPath = "/home/jared/Programs/mahppybird/Resources/GreatVibes-Regular.otf" 
