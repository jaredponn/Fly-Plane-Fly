module GameVars where

import Data.Stream
import Walls
import Linear.V2
import Input
import Foreign.C.Types
import Data.Stack

data Vars = Vars { vGameStateStack :: GameStack

                 , vPlayVars :: PlayVars

                 , score ::{-# UNPACK #-} !Int

                 , dt :: {-# UNPACK #-} !Float -- time it took for the frame to rendej
                 , camera :: {-# UNPACK #-} !(V2 CInt) -- camera position
                 , kInput :: Input 

                 , cGrav ::{-# UNPACK #-} !Float
                 , cJumpHeight :: {-# UNPACK #-} !Float
                 , cCamOffset :: {-# UNPACK #-} !(V2 Float)
                 , cRightVel :: {-# UNPACK #-} !Float
                 , cWallConf :: {-# UNPACK #-} !WallConfig
                 , cPlayerSize :: {-# UNPACK #-} !(V2 Float) }


data PlayVars = PlayVars { playerPos :: {-# UNPACK #-} !(V2 Float)
                         , vel :: {-# UNPACK #-} !Float
                         , wallStream :: Stream Wall }


data GameState = Menu
                | Play 
                | Pause 
                | GameOver 
                | Quit
                deriving Eq

type GameStack = Stack GameState
