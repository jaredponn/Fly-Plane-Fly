{-# LANGUAGE ScopedTypeVariables#-}
{-# LANGUAGE OverloadedStrings#-}
module InitGameVars ( initConf
                    , initVars
                    , defaultInput 
                    , defaultPlayerVars) where

import SDL ( V2 (..)
           , Point (..))
import qualified SDL
import qualified SDL.Font as TTF
import qualified SDL.Image as Image
import qualified SDL.Mixer as Mixer
import System.FilePath
import Foreign.C.Types
import Data.Stack
import Data.Stream as S

import GameVars 
import Walls
import Animations
import GameStateManager

resourcePath :: FilePath
resourcePath = "/home/jared/Programs/mahppybird/Resources" 

screenWidth, screenHeight :: CInt
screenWidth = 1280
screenHeight = 720

initVars :: IO Vars
initVars = do
        playvars <- initPlayVars
        return Vars { _vGameStateStack = stackPush stackNew Menu
                    , _vPlayVars = playvars
                    , _vRenderingVars = defaultRenderingVars
                    , _kInput = defaultInput
                    , _dt = 0
                    , _highScores = (0,0,0) }

initConf :: IO Config
initConf = do
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

        let guitextures = GUITextures{ _playBtnTexture = bgpic
                                      , _quitBtnTexture = bgpic
                                      , _playAgainBtnTexture = bgpic
                                     , _gameOverWindowTexture = bgpic }
            textures = Textures {_bgTexture = bgpic
                                , _playerSpriteSheet = playerpic
                                , _botWallTexture = botwallpic
                                , _topWallTexture = topwallpic
                                , _guiTextures = guitextures }
            animations = Animations { _playerJumpAnimation = playerjumpanimationsrcrects
                                    , _playerDeathAnimation = playerdeathanimationsrcrects
                                    , _playerIdleAnimation = playeridlesrcrects }
            sound = Sound { _bgMusicChannel = bgmusicchannel
                          , _jumpFx = jumpsfx}
            resources = Resources { _cFont = font
                                  , _cTextures = textures
                                  , _cAnimations = animations
                                  , _cSound = sound }

        return Config { cWindow = window
                      , cRenderer = renderer
                      , _cResources = resources }


playeridlesrcrects, playerjumpanimationsrcrects, playerdeathanimationsrcrects :: [AnimationSrcRect]
playeridlesrcrects = generateSrcRects (SDL.P (V2 0 0)) (V2 30 30) (V2 30 0) 3 AnimationType'Idle
playerjumpanimationsrcrects = generateSrcRects (SDL.P (V2 0 30)) (V2 30 30) (V2 30 0) 3 AnimationType'Jump
playerdeathanimationsrcrects = generateSrcRects (SDL.P (V2 0 60)) (V2 30 30) (V2 30 0) 3 AnimationType'Death


defaultInput :: Input 
defaultInput = Input { _isSpace = False
                     , _isEsc = False 
                     , _mousePos = V2 0 0
                     , _mousePress = False }

defaultPlayerVars :: Player
defaultPlayerVars = Player { _attributes = SDL.Rectangle (SDL.P (V2 50 100)) (V2 30 30)
                           , _yvel = 0
                           , _xvel = 125
                           , _cJumpHeight = (-700)
                           , _isPassingWall = False }

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
                       , allWallWidth = 100
                       , allWallSpacing = 175
                       , startingPos = 700 }


defaultRenderingVars :: RenderingVars 
defaultRenderingVars = RenderingVars { _playerAnimationHandler = createAnimationHandler (generateSrcRectStream playeridlesrcrects) 0.25
                              , _bgRect = SDL.Rectangle (SDL.P (V2 0 0)) (V2 (fromIntegral screenWidth) (fromIntegral screenHeight))
                              , _cameraPos = SDL.P $ V2 0 0
                              , _camOffset = (-100)}
