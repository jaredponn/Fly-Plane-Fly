module Input where

import SDL
import Foreign.C.Types
import Control.Monad.IO.Class (MonadIO(..))

data Input = Input { isSpace :: Bool 
                   , isEsc :: Bool
                   , mousePos :: V2 CInt
                   , mousePress :: Bool }

instance Show Input where
        show a = "space: " ++ (show . isSpace $ a) ++ " esc: " ++ (show . isEsc $ a)

class Monad m => HasInput m where
        updateInput :: m ()
        setInput :: Input -> m ()
        getInput :: m Input

eventIs :: Keycode  -- desired key
        -> Event  -- input event
        -> Bool
eventIs key event =
        case eventPayload event of
          KeyboardEvent keyboardEvent -> do
                  keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
                          keysymKeycode (keyboardEventKeysym keyboardEvent) == key
          _ -> False

