{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import SDL (($=)
           , V2 (..))
import Foreign.C.Types
import Control.Monad (unless)

import Game
import Walls

main :: IO ()
main = do
        SDL.initializeAll
        window <- SDL.createWindow "Mahppy Bird" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
        renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
        let cfg = Config { cWindow = window
                         , cRenderer = renderer
                         , cWindowSize = (screenWidth, screenHeight)

                         , cGrav = 0.001
                         , cJumpHeight = (-0.5)
                         , cRightVel = 0.3
                         , cCamOffset = (-100)
                         , cResources = undefined
                         , cWallConf = wallConf
                         , cPlayerSize = V2 20 20}

        wallstream <- createWallStream wallConf

        let vars = Vars { playerPos  = V2 50 2
                        , vel = 0.0001
                        , dt = 0
                        , score = 0
                        , camera = V2 0 0
                        , kInput = undefined
                        , wallStream = wallstream}
        runMahppyBird cfg vars loop

        return ()

screenWidth, screenHeight :: CInt
screenWidth = 1280
screenHeight = 720

wallConf :: WallConfig
wallConf =  WallConfig { allUppperWallRngBounds = (0, 0.7)
                           , allGapSize = 0.25
                           , allWallWidth = 100
                           , allWallSpacing = 175
                           , startingPos = 800 }

