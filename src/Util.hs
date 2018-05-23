module Util where

import Foreign.C.Types
import Linear.V2
import System.Clock
import Aabb

type Time = Float

type Rectangle = (V2 Float, V2 Float) -- (x y of top left corner, width height)

roundV2 :: V2 Float -> V2 CInt
roundV2 (V2 a b) = V2 (CInt $ round a) (CInt $ round b)

convertToSeconds :: TimeSpec -> Float
convertToSeconds = (/ 1000000000.0) . fromIntegral . toNanoSecs
