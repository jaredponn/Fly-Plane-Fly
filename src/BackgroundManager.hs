{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleContexts #-}

module BackgroundManager where

import Control.Monad.State (MonadState (..)
                           , StateT (..)
                           , gets
                           , modify)

import GameVars
import SDL
import Foreign.C.Types

class Monad m => BackgroundManager m where
        getBgRect :: m (Rectangle Float)
        setBgRectPos :: V2 Float -> m ()
        translateBgRect :: V2 Float -> m ()

{- instance BackgroundManager MahppyBird where -}
{-         getBgRect :: MonadState Vars m => m (Rectangle Float) -}
{-         getBgRect = bgRect <$> gets animationVars -}
{-  -}
{-         setBgRectPos :: MonadState Vars m => V2 Float -> m () -}
{-         setBgRectPos npos = do -}
{-                 animationvars <- gets animationVars -}
{-                 let Rectangle (P (V2 _ _)) lengths =  bgRect animationvars -}
{-                     nrect = Rectangle (P npos) lengths -}
{-                 modify (\v -> v {animationVars = animationvars {bgRect = nrect}}) -}
{-  -}
{-  -}
{-         translateBgRect :: (BackgroundManager m, MonadState Vars m) => V2 Float -> m () -}
{-         translateBgRect translation = do -}
{-                 Rectangle (P pos) lengths <- getBgRect -}
{-                 let npos = pos + translation -}
{-                 setBgRectPos npos -}
{-  -}
