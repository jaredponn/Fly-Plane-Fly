module GameVars where

import Data.Stream
import Walls
import Linear.V2
import Input
import Foreign.C.Types

data Vars = Vars { playerPos :: {-# UNPACK #-} !(V2 Float)
                 , vel :: {-# UNPACK #-} !Float
                 , score ::{-# UNPACK #-} !Int
                 , dt :: {-# UNPACK #-} !Float
                 , camera :: {-# UNPACK #-} !(V2 CInt)
                 , kInput :: Input 
                 , wallStream :: Stream Wall 

                 , cGrav ::{-# UNPACK #-} !Float
                 , cJumpHeight :: {-# UNPACK #-} !Float
                 , cCamOffset :: {-# UNPACK #-} !Float
                 , cRightVel :: {-# UNPACK #-} !Float
                 , cWallConf :: {-# UNPACK #-} !WallConfig
                 , cPlayerSize :: {-# UNPACK #-} !(V2 Float) }


