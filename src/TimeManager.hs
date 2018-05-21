module TimeManager where

import System.Clock

class Monad m => TimeManager m where
        getRealTime :: m (TimeSpec)
        threadDelay :: Int -> m ()
        setdt :: Float -> m ()

convertToSeconds :: TimeSpec -> Float
convertToSeconds = (/ 1000000000.0) . fromIntegral . toNanoSecs
