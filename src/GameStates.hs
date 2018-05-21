module GameStates where

import Data.Stack
import GameVars

data GameStates = Menu
                | Play Vars
                | Pause Int -- Int is the current score
                | GameOver Int -- Int is the final score
                | Quit

type GameStack = Stack GameStates
