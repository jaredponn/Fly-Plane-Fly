module Logger where


class Monad m => Logger m where
        logText :: String -> m ()
        logToFile :: FilePath -> String -> m ()

