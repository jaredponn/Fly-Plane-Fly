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
