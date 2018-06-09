module Util where

import SDL
import Foreign.C.Types
import System.Clock

type Time = Float

roundV2 :: V2 Float -> V2 CInt
roundV2 (V2 a b) = V2 (CInt $ round a) (CInt $ round b)

roundSDLRect :: Rectangle Float -> Rectangle CInt
roundSDLRect = (\(Rectangle (P (V2 x y)) (V2 w h)) -> Rectangle (P (V2 (round x) (round y))) (V2 (round w) (round h)))

