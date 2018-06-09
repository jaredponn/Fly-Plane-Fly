{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module TimeManager where

import System.Clock
import Control.Monad.State
import Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Concurrent (threadDelay)
import Control.Lens

import GameVars

class Monad m => TimeManager m where
        getRealTime :: m (TimeSpec)
        threadDelay :: Int -> m ()
        setdt :: Float -> m ()
        getdt :: m (Float)

instance TimeManager FlyPlaneFly where
        getRealTime :: MonadIO m => m (System.Clock.TimeSpec)
        getRealTime = liftIO . System.Clock.getTime $ System.Clock.Realtime

        threadDelay :: MonadIO m => Int -> m ()
        threadDelay = liftIO . Control.Concurrent.threadDelay 

        setdt :: MonadState Vars m => Float -> m ()
        setdt = (.=) dt 

        getdt :: MonadState Vars m => m (Float)
        getdt = use dt

-- using this method will ensure that the dt is always in milliseconds
setdtFromTimeSpec :: TimeManager m => TimeSpec -> TimeSpec -> m ()
setdtFromTimeSpec t1 t0 = setdt . convertTimeSpecTo nanoSecsToMilliSecs $ System.Clock.diffTimeSpec t1 t0


{- Unit conversion functions -}
convertTimeSpecTo :: Num a => (a -> a)  -- function to convert the number. This MUST be from nano seconds TO something else
                  -> TimeSpec  -- raw time from System.Clock
                  -> a
convertTimeSpecTo f timespec = f . fromIntegral . toNanoSecs $ timespec 

nanoSecsToMilliSecs :: Fractional a => a -> a
nanoSecsToMilliSecs = (*0.000001)

secsToMicroSecs :: Num a => a -> a
secsToMicroSecs = (*1000000)

milliSecsToSecs :: Fractional a => a -> a
milliSecsToSecs = (*0.001)
