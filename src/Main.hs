{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-} 
module Main where

import qualified SDL
import qualified SDL.Font as TTF
import qualified SDL.Image as Image
import qualified SDL.Mixer as Mixer
import Control.Lens

import MahppyBird
import GameVars

import InitGameVars

main :: IO ()
main = do
        SDL.initializeAll
        TTF.initialize
        Mixer.openAudio Mixer.defaultAudio 2048
        Mixer.setChannels 8

        initconf <- initConf
        initvars <- initVars

        runMahppyBird initconf initvars loop

        SDL.destroyRenderer $ cRenderer initconf
        SDL.destroyWindow $ cWindow initconf

        Mixer.free $ view (cResources.cSound.jumpFx) initconf 
        Mixer.closeAudio
        Mixer.quit

        Image.quit
        TTF.quit
        SDL.quit

        return ()


