{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-} 
module Main where

import qualified SDL
import qualified SDL.Font as TTF
import qualified SDL.Image as Image
import qualified SDL.Mixer as Mixer

import FlyPlaneFly
import Resources
import GameVars

main :: IO ()
main = do
        SDL.initializeAll
        TTF.initialize
        Mixer.openAudio Mixer.defaultAudio 2048
        Mixer.setChannels 8

        initconf <- initConf
        initvars <- initVars

        runFlyPlaneFly initconf initvars loop

        freeResources $ _cResources initconf

        SDL.destroyRenderer $ cRenderer initconf
        SDL.destroyWindow $ cWindow initconf
        Mixer.closeAudio

        Mixer.quit
        Image.quit
        TTF.quit
        SDL.quit
