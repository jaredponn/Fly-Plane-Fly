{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-} 
module Main where

import qualified SDL
import SDL (($=)
           , V2 (..)
           , Point (..))
import qualified SDL.Font as TTF
import qualified SDL.Image as Image
import qualified SDL.Mixer as Mixer
import Foreign.C.Types
import Control.Monad (unless)
import Data.Stack
import System.FilePath

import MahppyBird
import Walls
import GameVars
import Animations

main :: IO ()
main = do
        SDL.initializeAll
        TTF.initialize
        Mixer.openAudio Mixer.defaultAudio 2048
        Mixer.setChannels 8

        window <- SDL.createWindow "Mahppy Bird" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
        renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

        font <- TTF.load (resourcePath </> "GreatVibes-Regular.otf") 30
        playerpic <- Image.load (resourcePath </> "playerspritesheet.png") >>= SDL.createTextureFromSurface renderer 
        botwallpic <- Image.load (resourcePath </> "botwall.jpeg") >>= SDL.createTextureFromSurface renderer 
        topwallpic <- Image.load (resourcePath </> "topwall.jpg") >>= SDL.createTextureFromSurface renderer 
        bgpic <- Image.load (resourcePath </> "bg.jpg") >>= SDL.createTextureFromSurface renderer 

        (jumpsfx :: Mixer.Chunk) <- Mixer.load (resourcePath </> "fx.wav")
        (music :: Mixer.Chunk) <- Mixer.load (resourcePath </> "music.wav")
        bgmusicchannel <- Mixer.fadeInLimit Mixer.NoLimit 0 Mixer.Forever 1000 music

        let playeridlesrcrects = generateSrcRects (SDL.P (V2 0 0)) (V2 30 30) (V2 30 0) 3 AnimationType'Idle
            playerjumpanimationsrcrects = generateSrcRects (SDL.P (V2 0 30)) (V2 30 30) (V2 30 0) 3 AnimationType'Jump
            playerdeathanimationsrcrects = generateSrcRects (SDL.P (V2 0 60)) (V2 30 30) (V2 30 0) 3 AnimationType'Death
            guitextures = GUITextures{ _playBtnTexture = bgpic
                                      , _quitBtnTexture = bgpic
                                      , _playAgainBtnTexture = bgpic
                                     , _gameOverWindowTexture = bgpic}
            textures = Textures {_bgTexture = bgpic
                                , _playerSpriteSheet = playerpic
                                , _botWallTexture = botwallpic
                                , _topWallTexture = topwallpic
                                , _guiTextures = guitextures }
            animations = Animations { _playerJumpAnimation = playerjumpanimationsrcrects
                                    , _playerDeathAnimation = playerdeathanimationsrcrects
                                    , _playerIdleAnimation = playeridlesrcrects}
            sound = Sound { _bgMusicChannel = bgmusicchannel
                          , _jumpFx = jumpsfx}
            resources = Resources { _cFont = font
                                  , _cTextures = textures
                                  , _cAnimations = animations
                                  , _cSound = sound }

            cfg = Config { cWindow = window
                         , cRenderer = renderer
                         , _cResources = resources}


        wallstream <- createWallStream wallConf

        let playervars = Player { _attributes = SDL.Rectangle (SDL.P (V2 50 2)) (V2 30 30)
                                , _yvel = 0
                                , _xvel = 125
                                , _cJumpHeight = (-700)
                                , _isPassingWall = False }
            playvars = PlayVars { _player = playervars
                                , _wallStream = wallstream
                                , _cGrav = 2900
                                , _cWallConf = wallConf
                                , _score = 0 }
            kinput = Input { _isSpace = False
                           , _isEsc = False 
                           , _mousePos = V2 0 0
                           , _mousePress = False }

            playersrcidlerectstream = generateSrcRectStream playeridlesrcrects 
            renderingvars = RenderingVars { _playerAnimationHandler =  createAnimationHandler playersrcidlerectstream 0.25
                                          , _bgRect = SDL.Rectangle (SDL.P (V2 0 0)) (V2 (fromIntegral screenWidth) (fromIntegral screenHeight))
                                          , _cameraPos = SDL.P $ V2 0 0
                                          , _camOffset = (-100)}

            vars = Vars { _vGameStateStack = stackPush stackNew Menu
                        , _vPlayVars = playvars
                        , _vRenderingVars = renderingvars
                        , _kInput = kinput
                        , _dt = 0
                        , _highScores = (0,0,0) }


        runMahppyBird cfg vars loop

        SDL.destroyRenderer renderer
        SDL.destroyWindow window

        Mixer.closeAudio
        Mixer.free music
        Mixer.free jumpsfx
        Mixer.quit

        Image.quit
        TTF.quit
        SDL.quit

        return ()

screenWidth, screenHeight :: CInt
screenWidth = 1280
screenHeight = 720

wallConf :: WallConfig
wallConf =  WallConfig { allUppperWallRngBounds = (0.1, 0.44)
                       , startingGapSize = 0.40
                       , gapSizeChangeRate = (-0.02)
                       , finalGapSize = 0.22
                       , allWallWidth = 100
                       , allWallSpacing = 175
                       , startingPos = 700 }

resourcePath :: FilePath
resourcePath = "/home/jared/Programs/mahppybird/Resources" 
