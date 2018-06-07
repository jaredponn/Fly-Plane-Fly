{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE OverloadedStrings#-}
module InitGameVars ( initConf
                    , initVars
                    , defaultInput 
                    , defaultPlayerVars) where

import SDL ( V2 (..)
           , Point (..)
           , ($=))
import qualified SDL
import qualified SDL.Font as TTF
import qualified SDL.Image as Image
import qualified SDL.Mixer as Mixer
import System.FilePath
import Foreign.C.Types
import Data.Stream as S

import GameVars 
import Walls
import Animations

resourcePath :: FilePath
resourcePath = "/home/jared/Programs/Fly-Plane-Fly/Resources/" 

screenWidth, screenHeight :: CInt
screenWidth = 1280
screenHeight = 720

initVars :: IO Vars
initVars = do
        playvars <- initPlayVars
        return Vars { _vSceneState = Menu
                    , _vPlayVars = playvars
                    , _vRenderingVars = defaultRenderingVars
                    , _kInput = defaultInput
                    , _dt = 0
                    , _highScore = 0 }

initConf :: IO Config
initConf = do
        window <- SDL.createWindow "Mahppy Bird" SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }
        renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
        SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend

        scorefont <- TTF.load (resourcePath </> "FFFFORWA.TTF") 72
        highscorefont <- TTF.load (resourcePath </> "FFFFORWA.TTF") 35
        playerpic <- Image.load (resourcePath </> "playerspritesheet.png") >>= SDL.createTextureFromSurface renderer 
        botwallpic <- Image.load (resourcePath </> "botwall.png") >>= SDL.createTextureFromSurface renderer 
        topwallpic <- Image.load (resourcePath </> "topwall.png") >>= SDL.createTextureFromSurface renderer 
        bgpic <- Image.load (resourcePath </> "bg.png") >>= SDL.createTextureFromSurface renderer 
        titlebgpic <- Image.load (resourcePath </> "titlebg.png") >>= SDL.createTextureFromSurface renderer 
        pressspacetojumppic <- Image.load (resourcePath </> "pressspacetojump.png") >>= SDL.createTextureFromSurface renderer 

        mutepic <- Image.load (resourcePath </> "mute.png") >>= SDL.createTextureFromSurface renderer 
        mutedpic <- Image.load (resourcePath </> "muted.png") >>= SDL.createTextureFromSurface renderer 

        playmainmenupic <- Image.load (resourcePath </> "playmainmenu.png") >>= SDL.createTextureFromSurface renderer 
        quitmainmenupic <- Image.load (resourcePath </> "quitmainmenu.png") >>= SDL.createTextureFromSurface renderer 

        playagaingameoverpic <- Image.load (resourcePath </> "playagaingameover.png") >>= SDL.createTextureFromSurface renderer 
        quitgameoverpic <- Image.load (resourcePath </> "quitgameover.png") >>= SDL.createTextureFromSurface renderer 
        gameoverwindowpic <- Image.load (resourcePath </> "gameoverwindow.png") >>= SDL.createTextureFromSurface renderer 

        (jumpsfx :: Mixer.Chunk) <- Mixer.load (resourcePath </> "fx.wav")
        (crashfx :: Mixer.Chunk) <- Mixer.load (resourcePath </> "crash.wav")
        (music :: Mixer.Chunk) <- Mixer.load (resourcePath </> "music.wav")
        bgmusicchannel <- Mixer.fadeInLimit Mixer.NoLimit 0 Mixer.Forever 1000 music

        let guitextures = GUITextures{ _playBtnTexture = playmainmenupic 
                                      , _quitBtnTexture = quitmainmenupic
                                      , _playAgainBtnTexture = playagaingameoverpic 
                                      , _quitGameOverBtnTexture = quitgameoverpic
                                      , _gameOverWindowTexture = gameoverwindowpic
                                      , _pressSpacetoJumpTexture = pressspacetojumppic 
                                      , _titleScreenbg = titlebgpic
                                     , _muteTexture = mutepic
                                     , _mutedTexture = mutedpic }
            textures = Textures {_bgTexture = bgpic
                                , _playerSpriteSheet = playerpic
                                , _botWallTexture = botwallpic
                                , _topWallTexture = topwallpic
                                , _guiTextures = guitextures }
            animations = Animations { _playerJumpAnimation = playerjumpanimationsrcrects
                                    , _playerDeathAnimation = playerdeathanimationsrcrects
                                    , _playerIdleAnimation = playeridlesrcrects }
            sound = Sound { _bgMusicChannel = bgmusicchannel
                          , _jumpFx = jumpsfx 
                          , _crashFx = crashfx}
            fonts = Fonts { _scoreFont = scorefont
                          , _highScoreFont = highscorefont}
            resources = Resources { _cFont = fonts
                                  , _cTextures = textures
                                  , _cAnimations = animations
                                  , _cSound = sound }

        return Config { cWindow = window
                      , cRenderer = renderer
                      , _cResources = resources }


playeridlesrcrects, playerjumpanimationsrcrects, playerdeathanimationsrcrects :: [AnimationSrcRect]
playeridlesrcrects = generateSrcRects (SDL.P (V2 0 0)) (V2 60 30) (V2 60 0) 6 AnimationType'Idle
playerjumpanimationsrcrects = generateSrcRects (SDL.P (V2 0 30)) (V2 60 30) (V2 60 0) 3 AnimationType'Jump
playerdeathanimationsrcrects = generateSrcRects (SDL.P (V2 0 60)) (V2 60 30) (V2 60 0) 5 AnimationType'Death


defaultInput :: Input 
defaultInput = Input { _isSpace = False
                     , _isEsc = False 
                     , _mousePos = V2 0 0
                     , _mousePress = False }

defaultPlayerVars :: Player
defaultPlayerVars = Player { _attributes = SDL.Rectangle (SDL.P (V2 0 270)) (V2 60 30)
                           , _yvel = 0
                           , _xvel = 275
                           , _cJumpHeight = (-700)
                           , _isPassingWall = False
                           , _angle = 0}

initPlayVars :: IO PlayVars
initPlayVars = do
        wallstream <- initWallStream
        return PlayVars { _player = defaultPlayerVars
                        , _wallStream = wallstream
                        , _cGrav = 2900
                        , _cWallConf = initWallConf
                        , _score = 0 }

initWallStream :: IO (S.Stream Wall)
initWallStream = createWallStream initWallConf

initWallConf :: WallConfig
initWallConf =  WallConfig { allUppperWallRngBounds = (0.1, 0.44)
                       , startingGapSize = 0.40
                       , gapSizeChangeRate = (-0.02)
                       , finalGapSize = 0.22
                       , allWallWidth = 150
                       , allWallSpacing = 200
                       , startingPos = 700 }


defaultRenderingVars :: RenderingVars 
defaultRenderingVars = RenderingVars { _playerAnimationHandler = createAnimationHandler (generateSrcRectStream playeridlesrcrects) 0.1
                                     , _cameraPos = SDL.P $ V2 0 0
                                     , _camOffset = (-100)
                                     , _transitionOpacity = 100 }
