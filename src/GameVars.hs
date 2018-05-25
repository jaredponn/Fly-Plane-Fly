{-# LANGUAGE GeneralizedNewtypeDeriving #-} 
module GameVars where

import Data.Stream
import Walls
import Linear.V2
import Foreign.C.Types
import Data.Stack
import qualified SDL
import qualified SDL.Font as TTF
import Control.Monad.Reader (MonadReader (..)
                            , ReaderT (..))
import Control.Monad.State (MonadState (..)
                           , StateT (..))
import Control.Monad.IO.Class (MonadIO(..))

-- ReaderT Environment Monad ReturnedVal
newtype MahppyBird a = MahppyBird (ReaderT Config (StateT Vars IO) a) 
        deriving (Functor, Applicative, Monad, MonadReader Config, MonadState Vars, MonadIO)
                

data Config = Config { cWindow :: SDL.Window
                     , cRenderer :: SDL.Renderer
                     , cResources :: Resources }

data Resources = Resources { cFont :: TTF.Font
                           , bgTexture :: SDL.Texture
                           , playerTexture :: SDL.Texture
                           , botWallTexture :: SDL.Texture
                           , topWallTexture :: SDL.Texture }

data Vars = Vars { vGameStateStack :: GameStack

                 , vPlayVars :: PlayVars

                 , score ::{-# UNPACK #-} !Int

                 , dt :: {-# UNPACK #-} !Float -- time it took for the frame to render
                 , camera :: {-# UNPACK #-} !(SDL.Point V2 CInt) -- camera position
                 , kInput :: Input 

                 , cGrav ::{-# UNPACK #-} !Float
                 , cJumpHeight :: {-# UNPACK #-} !Float
                 , cCamOffset :: {-# UNPACK #-} !(V2 Float)
                 , cRightVel :: {-# UNPACK #-} !Float
                 , cWallConf :: {-# UNPACK #-} !WallConfig }


data Input = Input { isSpace :: Bool 
                   , isEsc :: Bool
                   , mousePos :: V2 CInt
                   , mousePress :: Bool }
                   deriving Show

-- http://lazyfoo.net/tutorials/SDL/30_scrolling/index.php
-- https://hackage.haskell.org/package/sdl2-2.4.0.1/docs/SDL-Raw-Types.html

instance Show Vars where
        show vars = 
                   {- "Playerpos: " ++ show (playerPos vars) ++ "\n" -}
                {- ++ "Velocity: " ++ show (vel vars) ++ "\n" -}
                "\n"
                ++ "dt: " ++ show (dt vars) ++ "\n"
                ++ "FPS: " ++ show (10000.001 / (dt vars)) ++ "\n"
                ++ "camera: " ++ show (camera vars) ++ "\n"
                ++ "kInput: " ++ show (kInput vars) ++ "\n"


data PlayVars = PlayVars { player :: SDL.Rectangle Float
                         , vel :: {-# UNPACK #-} !Float
                         , wallStream :: Stream Wall
                         , isPassingWall :: Bool}


data GameState = Menu
                | Play 
                | Pause 
                | GameOver 
                | Quit
                deriving Eq

type GameStack = Stack GameState
