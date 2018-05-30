{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module ScoreManager where

import Control.Monad.State
import Control.Lens

import GameVars

class Monad m => ScoreManager m where
        incrementScore :: m ()
        getScore :: m (Int)
        resetScore :: m ()

        -- returns place 1, 2, or 3
        scorePlacing :: Int -> m (Maybe Int)
        -- takes the score it replaces and modifies the internal state
        modifyHighScore :: Maybe Int  -- how high the new score ranks
                        -> Int  -- the new score
                        -> m ()
        getHighScores :: m(Int, Int, Int)

instance ScoreManager MahppyBird where
        getScore :: MonadState Vars m => m (Int)
        getScore = use $ vPlayVars.score

        incrementScore :: MonadState Vars m => m ()
        incrementScore = vPlayVars.score += 1

        resetScore :: MonadState Vars m => m ()
        resetScore = vPlayVars.score .= 0

        scorePlacing :: MonadState Vars m => Int -> m (Maybe Int)
        scorePlacing score = do
                scores <- use highScores
                return $ f score scores 
                        where
                                f n (a,b,c)
                                  | n >= a = Just 1
                                  | n >= b = Just 2
                                  | n >= c = Just 3
                                  | otherwise = Nothing


        modifyHighScore :: MonadState Vars m => Maybe Int -> Int -> m ()
        modifyHighScore (Just place) val = highScores %= f val place
                where 
                        f val place (a,b,c) 
                          | place == 1 = (val,b,c)
                          | place == 2 = (a,val,c)
                          | place == 3 = (a,b,val)
                          | otherwise = (a,b,c)
        modifyHighScore Nothing _ = return ()

        getHighScores :: MonadState Vars m => m (Int, Int, Int)
        getHighScores = use highScores


