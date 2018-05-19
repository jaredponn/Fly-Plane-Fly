module Aabb ( Aabb (..)
            , hitTest 
            , hitTestX
            , hitTestY
            ) where

import Linear.V2

data Aabb = Aabb { pMin :: {-# UNPACK #-} !(V2 Float)
                 , pMax :: {-# UNPACK #-} !(V2 Float) }

-- see this page for more on Axis aligned bounding boxes 
{- https://developer.mozilla.org/en-US/docs/Games/Techniques/3D_collision_detection -}
hitTest :: Aabb -> Aabb -> Bool
hitTest (Aabb (V2 xmin0 ymin0) (V2 xmax0 ymax0)) (Aabb (V2 xmin1 ymin1) (V2 xmax1 ymax1)) = 
        (xmin0 <= xmax1 && xmax0 >= xmin1) &&
        (ymin0 <= ymax1 && ymax0 >= ymin1)

hitTestX :: Aabb -> Aabb -> Bool
hitTestX (Aabb (V2 xmin0 _) (V2 xmax0 _)) (Aabb (V2 xmin1 _) (V2 xmax1 _)) = 
        (xmin0 <= xmax1 && xmax0 >= xmin1) 

hitTestY :: Aabb -> Aabb -> Bool
hitTestY (Aabb (V2 _ ymin0) (V2 _ ymax0)) (Aabb (V2 _ ymin1) (V2 _ ymax1)) = 
        (ymin0 <= ymax1 && ymax0 >= ymin1)
