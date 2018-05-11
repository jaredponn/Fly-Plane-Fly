{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import SDL (($=)
           , V2 (..))
import Foreign.C.Types
import Control.Monad (unless)

import Game

main :: IO ()
main = do
        SDL.initializeAll
        window <- SDL.createWindow "Mahppy Bird" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
        renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
        let cfg = Config { cWindow = window
                         , cRenderer = renderer

                         , cGrav = 0.001
                         , cJumpHeight = (-1)
                         , cRightVel = 0.03
                         , cCamOffset = (-100)
                         , cResources = undefined}
        let vars = Vars { playerPos  = V2 50 2
                        , vel = 0.0001
                        , dt = 0
                        , camera = V2 0 0
                        , kInput = undefined}
        runMahppyBird cfg vars loop

        return ()

screenWidth, screenHeight :: CInt
screenWidth = 1280
screenHeight = 720

{- loop :: SDL.Renderer -> IO () -}
{- loop renderer = do -}
{-         events <- SDL.pollEvents -}
{-         let eventIsQPress event =  -}
{-                     case SDL.eventPayload event of -}
{-                       SDL.KeyboardEvent keyboardEvent -> -}
{-                               SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed && -}
{-                               SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ -}
{-                       _ -> False -}
{-         let qPressed = any eventIsQPress events -}
{-  -}
{-         SDL.rendererDrawColor renderer $= SDL.V4 0 0 255 255 -}
{-         SDL.clear renderer -}
{-         SDL.present renderer -}
{-         unless qPressed (loop renderer) -}
