{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module Physics where

import Control.Monad.State
import Control.Lens
import Linear.V2

import GameVars
import PlayerManager
import TimeManager
import Logger

class Monad m => Physics m where
        applyGrav :: m ()
        setGrav :: Float -> m ()

        addYVel :: Float -> m ()
        setYVel :: Float -> m ()

        applyYVel :: m ()
        applyXVel :: m ()

instance Physics FlyPlaneFly where
        -- converts the gravity into velocity 
        applyGrav :: (MonadState Vars m, PlayerManager m, TimeManager m) => m  ()
        applyGrav = do 
                acc <- use $ vPlayVars.gravity
                ndt <- getdt
                curvel <- getPlayerYVel
                setPlayerYVel $ curvel + ndt * acc

        setGrav :: MonadState Vars m => Float -> m ()
        setGrav ngrav = do 
                vPlayVars.gravity .= ngrav

        -- allows the addition of any given velocity number to the current velocity
        addYVel :: PlayerManager m => Float -> m  ()
        addYVel dv = do 
                curvel <- getPlayerYVel
                setPlayerYVel $ curvel + dv

        -- sets the current velocity to a new one
        setYVel :: PlayerManager m => Float -> m  ()
        setYVel nv = do 
                setPlayerYVel nv
        
        -- applies the velocity to the position by using dt to update the y position
        applyYVel :: (MonadState Vars m, PlayerManager m, TimeManager m) => m ()
        applyYVel = do
                curvel <- getPlayerYVel
                curdt <-  getdt
                translatePlayer $ V2 0 (curvel * curdt)

        -- applies the velocity to the position by using dt to update the y position
        applyXVel :: (Logger m, MonadState Vars m, PlayerManager m, TimeManager m) => m ()
        applyXVel = do
                xrv <- getPlayerXVel
                curdt <-  getdt
                translatePlayer $ V2 (xrv * curdt) 0
