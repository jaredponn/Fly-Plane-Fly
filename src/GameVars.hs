{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
{-# LANGUAGE TemplateHaskell #-}
module GameVars where

import Data.Stream
import Linear.V2
import Foreign.C.Types
import Data.Stack
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

data Config = Config { cWindow :: SDL.Window
                     , cRenderer :: SDL.Renderer
                     , cResources :: Resources }

data Resources = Resources { cFont :: TTF.Font
                           , bgTexture :: SDL.Texture
                           , playerTexture :: SDL.Texture
                           , playerJumpAnimation :: [AnimationSrcRect]
                           , botWallTexture :: SDL.Texture
                           , topWallTexture :: SDL.Texture
                           , bgMusicChannel :: Mixer.Channel
                           , jumpFx :: Mixer.Chunk }
data Player = Player { _attributes ::{-# UNPACK #-} !(SDL.Rectangle Float)
                     , _yvel ::{-# UNPACK #-} !Float 
                     , _xvel ::{-# UNPACK #-} !Float
                     , _cJumpHeight ::{-# UNPACK #-} !Float
                     , _isPassingWall ::{-# UNPACK #-} !Bool
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
                   , _mousePress ::{-# UNPACK #-} !(Bool) }
                   deriving Show
makeLenses ''Input

data RenderingVars = RenderingVars { _playerAnimationHandler :: AnimationHandler
                                   , _bgRect :: SDL.Rectangle Float
                                   , _cameraPos :: {-# UNPACK #-} !(SDL.Point V2 CInt) -- camera position
                                   , _camOffset :: {-# UNPACK #-} !(V2 Float)
                                   } deriving Show
makeLenses ''RenderingVars

data GameState = Menu
                | Play 
                | Pause 
                | GameOver 
                | Quit
                deriving (Eq, Show)

type GameStack = Stack GameState

data Vars = Vars { _vGameStateStack :: GameStack 
                 , _vPlayVars :: PlayVars
                 , _vRenderingVars :: RenderingVars 
                 , _kInput :: Input 
                 , _dt :: {-# UNPACK #-} !Float -- time it took for the frame to render
                 }  deriving Show

makeLenses ''Vars

-- ReaderT Environment Monad ReturnedVal
newtype MahppyBird a = MahppyBird (ReaderT Config (StateT Vars IO) a) 
        deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO)
