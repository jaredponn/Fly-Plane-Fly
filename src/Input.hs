{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module Input where

import SDL
import Foreign.C.Types
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader
import Control.Monad.State

import GameVars

eventIs :: Keycode  -- desired key
        -> Event  -- input event
        -> Bool
eventIs key event =
        case eventPayload event of
          KeyboardEvent keyboardEvent -> do
                  keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
                          keysymKeycode (keyboardEventKeysym keyboardEvent) == key
          _ -> False

class Monad m => HasInput m where
        updateInput :: m ()
        setInput :: Input -> m ()
        getInput :: m Input

instance HasInput MahppyBird where
        updateInput :: (HasInput m, MonadIO m, MonadState Vars m) => m ()
        updateInput = do
                events <- SDL.pollEvents
                SDL.P mousepos <- liftIO  SDL.getAbsoluteMouseLocation
                mousepress <- liftIO SDL.getMouseButtons
                let space = any (eventIs SDL.KeycodeSpace) events
                    esc = any (eventIs SDL.KeycodeEscape) events

                setInput Input { isSpace = space
                               , isEsc = esc 
                               , mousePos = mousepos
                               , mousePress = foldr ((||) . mousepress) False [SDL.ButtonLeft, SDL.ButtonMiddle, SDL.ButtonRight] }

        setInput :: (MonadState Vars m) => Input -> m ()
        setInput input = modify (\v -> v { kInput = input })

        getInput :: (MonadState Vars m) => m Input
        getInput = gets kInput

