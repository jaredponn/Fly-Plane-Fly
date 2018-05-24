{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import SDL (($=)
           , V2 (..)
           , Point (..))
import qualified SDL.Font as TTF
import qualified SDL.Image as Image
import Foreign.C.Types
import Control.Monad (unless)
import Data.Stack
import System.FilePath

import MahppyBird
import Walls
import GameVars

main :: IO ()
main = do
        SDL.initializeAll
        TTF.initialize

        window <- SDL.createWindow "Mahppy Bird" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
        renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

        font <- TTF.load (resourcePath </> "GreatVibes-Regular.otf") 30
        playerpic <- Image.load (resourcePath </> "player.png") >>= SDL.createTextureFromSurface renderer 
        botwallpic <- Image.load (resourcePath </> "botwall.jpeg") >>= SDL.createTextureFromSurface renderer 
        topwallpic <- Image.load (resourcePath </> "topwall.jpg") >>= SDL.createTextureFromSurface renderer 
        bgpic <- Image.load (resourcePath </> "bg.jpg") >>= SDL.createTextureFromSurface renderer 

        let cfg = Config { cWindow = window
                         , cRenderer = renderer
                         , cResources = Resources { cFont = font
                                                  , playerTexture =  playerpic
                                                  , botWallTexture = botwallpic
                                                  , topWallTexture = topwallpic
                                                  , bgTexture = bgpic} }


        wallstream <- createWallStream wallConf

        let pvars = PlayVars { player  = SDL.Rectangle (SDL.P (V2 50 2)) (V2 30 30)
                               , vel = 0.0001
                               , wallStream = wallstream }
            vars = Vars { vGameStateStack = stackPush stackNew Menu
                        , vPlayVars = pvars
                        , dt = 0
                        , score = 0
                        , camera = SDL.P $ V2 0 0
                        , kInput = undefined

                        , cGrav = 2900
                        , cJumpHeight = (-700)
                        , cRightVel = 125
                        , cCamOffset = (-100)
                        , cWallConf = wallConf }

        runMahppyBird cfg vars loop

        SDL.destroyRenderer renderer
        SDL.destroyWindow window

        Image.quit
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

resourcePath :: FilePath
resourcePath = "/home/jared/Programs/mahppybird/Resources" 
