module Menu where

import Control.Monad.State


newtype MainMenu a = MainMenu ()

data Vars = Vars { a :: Int }
