{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module Logger where

import Control.Monad.IO.Class (MonadIO(..))

import TimeManager
import GameVars
import Util

class Monad m => Logger m where
        logText :: String -> m ()
        logToFile :: FilePath -> String -> m ()


instance Logger MahppyBird where
        logText :: (MonadIO m, TimeManager m) => String -> m ()
        logText str = do
                t <- convertToSeconds <$> getRealTime
                liftIO . putStrLn $ (show t) ++ ": " ++ str ++ "\n"

        logToFile :: (MonadIO m, TimeManager m) => FilePath -> String -> m ()
        logToFile path str = do
                t <- convertToSeconds <$> getRealTime
                liftIO $ appendFile path ((show t) ++ ": " ++ str ++ "\n")
