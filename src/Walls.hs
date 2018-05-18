module Walls 
        ( Wall (..)
        , WallConfig (..) 
        , createWallStream
        ) where

import Data.Stream as Stream
import System.Random
import Control.Monad.State.Lazy

-- Walls are defined by lengths a, b, g (gap) in a percent, x denotes its x position. Actual dimensions will be determined in the monad
{- 
 x       x   
 a |     a |
 | |     |_|
 |_|      _g
  _g     b |
 b |     | |

   |space|
-}
data Wall = Wall { upperWall :: Float -- percent of the size
                 , gap :: Float -- percent of the size
                 , lowerWall :: Float -- percent of the size
                 , xPos :: Float -- actual position
                 , wallWidth :: Float } -- actual width
                 deriving Show

data WallConfig = WallConfig { allUppperWallRngBounds :: {-# UNPACK #-} !(Float, Float)
                             , allGapSize :: {-# UNPACK #-} !Float
                             , allWallWidth :: {-# UNPACK #-} !Float
                             , allWallSpacing :: {-# UNPACK #-} !Float 
                             , startingPos :: {-# UNPACK #-} !Float
                             }

-- takes a range of floats and spits out a value between them
randPercent :: RandomGen g => (Float, Float) 
      -> State g Float
randPercent (lowerBound, upperBound) = do 
        generator <- get 
        let intBounds = (round (100 * lowerBound) :: Int, round (100 * upperBound) :: Int)
            (val, generator') = randomR intBounds generator
        put generator'
        return $ fromIntegral val / 100

createWall :: RandomGen g => WallConfig
           -> Float -- starting wall position
           -> g -- rng generator
           -> (Wall, g)
createWall conf startPos g = let (val, g') = runState (randPercent (allUppperWallRngBounds conf)) g
                              in ( Wall { upperWall = val
                                        , gap = allGapSize conf
                                        , lowerWall = 1 - (val + allGapSize conf)
                                        , xPos = startPos
                                        , wallWidth = allWallWidth conf}
                                , g' )

createNextWall :: RandomGen g => WallConfig
               -> (Wall, g) -- old wall
               -> (Wall, g)
createNextWall conf (old, g) = let (val, g') = runState (randPercent (allUppperWallRngBounds conf)) g
                                in ( Wall { upperWall = val
                                          , gap = allGapSize conf
                                          , lowerWall = 1 - (val + allGapSize conf)
                                          , xPos = (allWallSpacing conf) + (xPos old) + (wallWidth old) 
                                          , wallWidth = allWallWidth conf}
                                   , g' )

createWallStream :: WallConfig
                 -> IO (Stream Wall)
createWallStream conf = do
        seed <- getStdGen
        let firstWall = createWall conf (startingPos conf) seed
        return . Stream.map fst $ Stream.iterate (createNextWall conf) firstWall 


{- FUNCTIONS TO HELP WITH TESTING -}
testCreateWallStream conf = do
        a <- createWallStream conf
        return $ Stream.take 4 a


testWallConf :: WallConfig
testWallConf =  WallConfig { allUppperWallRngBounds = (0, 0.6)
                           , allGapSize = 0.2
                           , allWallWidth = 30
                           , allWallSpacing = 30
                           , startingPos = 30 }

