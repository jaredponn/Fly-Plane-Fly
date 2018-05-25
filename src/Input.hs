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

keyEventIs :: Keycode  -- desired key
        -> Event  -- input event
        -> Bool
keyEventIs key event =
        case SDL.eventPayload event of
          KeyboardEvent keyboardEvent -> do
                  keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
                          keysymKeycode (keyboardEventKeysym keyboardEvent) == key
          _ -> False

isMouseTap :: Event -> Bool
isMouseTap event = 
        case SDL.eventPayload event of
          MouseButtonEvent mouseEvent -> toInteger (mouseButtonEventClicks mouseEvent) > 0
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
                let space = any (keyEventIs SDL.KeycodeSpace) events
                    esc = any (keyEventIs SDL.KeycodeEscape) events
                    mousepress = any isMouseTap events

                setInput Input { isSpace = space
                               , isEsc = esc 
                               , mousePos = mousepos
                               , mousePress = mousepress}

        setInput :: (MonadState Vars m) => Input -> m ()
        setInput input = modify (\v -> v { kInput = input })

        getInput :: (MonadState Vars m) => m Input
        getInput = gets kInput
