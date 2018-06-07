{-# LANGUAGE BangPatterns #-}
module Walls 
        ( Wall (..)
        , WallConfig (..) 
        , createWallStream
        ) where

import Data.Stream as S
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

-- The percents of the sizes will be converted to the actual sizes in the game. The actual sizes depend on the window size, so here, it is abstractly expressed in terms of a percent
data Wall = Wall { upperWall :: {-# UNPACK #-} !Float -- percent of the size
                 , gap :: {-# UNPACK #-} !Float -- percent of the size
                 , lowerWall ::{-# UNPACK #-} !Float -- percent of the size
                 , xPos :: {-# UNPACK #-} !Float -- actual position
                 , wallWidth :: {-# UNPACK #-} !Float } -- actual width
                 deriving Show

data WallConfig = WallConfig { allUppperWallRngBounds :: {-# UNPACK #-} !(Float, Float)
                             , startingGapSize :: {-# UNPACK #-} !Float
                             , gapSizeChangeRate :: {-# UNPACK #-} !Float
                             , finalGapSize :: {-# UNPACK #-} !Float
                             , allWallWidth :: {-# UNPACK #-} !Float
                             , allWallSpacing :: {-# UNPACK #-} !Float 
                             , startingPos :: {-# UNPACK #-} !Float
                             } deriving Show

-- takes a range of floats and spits out a value between them
-- used to make random percents. E.g. (0, 1) -> 0.56
randPercent :: RandomGen g => (Float, Float) 
      -> State g Float
randPercent !(lowerBound, upperBound) = do 
        generator <- get 
        let intBounds = (round (100 * lowerBound) :: Int, round (100 * upperBound) :: Int)
            (val, generator') = randomR intBounds generator
        put generator'
        return $ fromIntegral val / 100

createWall :: RandomGen g => WallConfig
           -> Float -- starting wall position
           -> g -- initial rng generator
           -> (Wall, g)
createWall !conf !startPos !g = let (val, g') = runState (randPercent (allUppperWallRngBounds conf)) g
                              in ( Wall { upperWall = val
                                        , gap = startingGapSize conf
                                        , lowerWall = 1 - (val + startingGapSize conf)
                                        , xPos = startPos
                                        , wallWidth = allWallWidth conf}
                                , g' )

createNextWall :: RandomGen g => WallConfig
               -> (Wall, g) -- old wall
               -> (Wall, g)
createNextWall !conf !(old, g) = let (val, g') = runState (randPercent (allUppperWallRngBounds conf)) g
                                     gapsize = if gap old + gapSizeChangeRate conf <= finalGapSize conf
                                                then finalGapSize conf
                                                else gap old + gapSizeChangeRate conf
                                 in ( Wall { upperWall = val
                                           , gap = gapsize
                                           , lowerWall = 1 - (val + gapsize)
                                           , xPos = (allWallSpacing conf) + (xPos old) + (wallWidth old) 
                                           , wallWidth = allWallWidth conf}
                                   , g' )

createWallStream :: WallConfig
                 -> IO (Stream Wall)
createWallStream !conf = do
        seed <- getStdGen
        let firstWall = createWall conf (startingPos conf) seed
        setStdGen . snd $ firstWall -- sets a new global rng generator so each new wall stream created will be unique
        return . S.map fst $ S.iterate (createNextWall conf) firstWall 
