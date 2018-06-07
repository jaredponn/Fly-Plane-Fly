{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module Input where

import SDL
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State
import Control.Lens

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
          MouseButtonEvent mouseEvent -> mouseButtonEventMotion mouseEvent == Released
          _ -> False

isSpaceTap :: Event -> Bool
isSpaceTap event =
        case SDL.eventPayload event of
          KeyboardEvent keyboardEvent -> do
                  keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
                          SDL.keysymKeycode (keyboardEventKeysym keyboardEvent) == SDL.KeycodeSpace &&
                                  not (SDL.keyboardEventRepeat keyboardEvent)
          _ -> False

class Monad m => HasInput m where
        updateInput :: m ()
        setInput :: Input -> m ()
        getInput :: m Input

instance HasInput FlyPlaneFly where
        updateInput :: (HasInput m, MonadIO m, MonadState Vars m) => m ()
        updateInput = do
                events <- SDL.pollEvents
                SDL.P mousepos <- liftIO  SDL.getAbsoluteMouseLocation
                let space = any isSpaceTap events
                    esc = any (keyEventIs SDL.KeycodeEscape) events
                    mousepress = any isMouseTap events

                setInput Input { _isSpace = space
                               , _isEsc = esc 
                               , _mousePos = mousepos
                               , _mousePress = mousepress}

        setInput :: (MonadState Vars m) => Input -> m ()
        setInput input = kInput .= input

        getInput :: (MonadState Vars m) => m Input
        getInput = use kInput
