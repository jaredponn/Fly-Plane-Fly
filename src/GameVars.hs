{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE TemplateHaskell #-}
module GameVars where

import Data.Stream
import Linear.V2
import Foreign.C.Types
import qualified GHC.Word
import qualified SDL
import qualified SDL.Font as TTF
import qualified SDL.Mixer as Mixer
import Control.Monad.Reader (MonadReader (..)
                            , ReaderT (..))
import Control.Monad.State (MonadState (..)
                           , StateT (..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Lens

import Animations 
import Walls

data GUITextures = GUITextures { _playBtnTexture :: SDL.Texture
                               , _quitBtnTexture :: SDL.Texture
                               , _playAgainBtnTexture :: SDL.Texture
                               , _quitGameOverBtnTexture :: SDL.Texture
                               , _gameOverWindowTexture :: SDL.Texture
                               , _pressSpacetoJumpTexture :: SDL.Texture
                               , _titleScreenbg :: SDL.Texture
                               , _mutedTexture :: SDL.Texture
                               , _muteTexture :: SDL.Texture}
makeLenses ''GUITextures

data Textures = Textures { _bgTexture :: SDL.Texture
                         , _playerSpriteSheet :: SDL.Texture
                         , _botWallTexture :: SDL.Texture
                         , _topWallTexture :: SDL.Texture
                         , _guiTextures :: GUITextures
                         }
makeLenses ''Textures

data Animations = Animations { _playerIdleAnimation :: [AnimationSrcRect]
                             , _playerJumpAnimation :: [AnimationSrcRect]
                             , _playerDeathAnimation :: [AnimationSrcRect] }
makeLenses ''Animations

data Sound = Sound { _bgMusicChannel :: Mixer.Channel
                   , _jumpFx :: Mixer.Chunk 
                   , _crashFx :: Mixer.Chunk }
makeLenses ''Sound

data Fonts = Fonts { _scoreFont :: TTF.Font
                   , _highScoreFont :: TTF.Font }

makeLenses ''Fonts

data Resources = Resources { _cFont :: Fonts
                           , _cTextures :: Textures
                           , _cAnimations :: Animations
                           , _cSound :: Sound }
makeLenses ''Resources

data Config = Config { cWindow :: SDL.Window
                     , cRenderer :: SDL.Renderer
                     , _cResources :: Resources }
makeLenses ''Config

data Player = Player { _attributes ::{-# UNPACK #-} !(SDL.Rectangle Float)
                     , _yvel ::{-# UNPACK #-} !Float 
                     , _xvel ::{-# UNPACK #-} !Float
                     , _cJumpHeight ::{-# UNPACK #-} !Float
                     {- , _isPassingWall ::{-# UNPACK #-} !Bool -}
                     , _isPassingWall :: !Bool
                     , _angle ::{-# UNPACK #-} !CDouble
                     } deriving Show
makeLenses ''Player

data PlayVars = PlayVars { _player ::{-# UNPACK #-} !Player
                         , _wallStream :: Stream Wall
                         , _cGrav ::{-# UNPACK #-} !Float
                         , _cWallConf ::{-# UNPACK #-} !WallConfig
                         , _score ::{-# UNPACK #-} !Int
                         } 
instance Show PlayVars where
        show _ = ""

makeLenses ''PlayVars

data Input = Input { _isSpace :: Bool 
                   , _isEsc :: Bool
                   , _mousePos ::{-# UNPACK #-} !(V2 CInt)
                   , _mousePress :: !(Bool) }
                   {- , _mousePress ::{-# UNPACK #-} !(Bool) } -}
                   deriving Show
makeLenses ''Input

data RenderingVars = RenderingVars { _playerAnimationHandler :: AnimationHandler
                                   , _cameraPos :: {-# UNPACK #-} !(SDL.Point V2 CInt) -- camera position
                                   , _camOffset :: {-# UNPACK #-} !(V2 Float)
                                   , _transitionOpacity :: {-# UNPACK #-} !GHC.Word.Word8
                                   } deriving Show
makeLenses ''RenderingVars

data SceneState = Menu
               | PrePlay
               | Play 
               | Pause 
               | GameOver 
               | Quit
               deriving (Eq, Show)

data Vars = Vars { _vSceneState :: SceneState 
                 , _vPlayVars :: PlayVars
                 , _vRenderingVars :: RenderingVars 
                 , _kInput :: Input 
                 , _dt :: {-# UNPACK #-} !Float -- time it took for the frame to render
                 , _highScore :: {-# UNPACK #-} !Int -- high score
                 }  deriving Show

makeLenses ''Vars

-- ReaderT Environment Monad ReturnedVal
newtype MahppyBird a = MahppyBird (ReaderT Config (StateT Vars IO) a) 
        deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO)
