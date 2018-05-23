module Aabb ( Aabb (..)
            , hitTest 
            , aabbToRectangle
            , rectangleToAabb
            , pointHitTest
            , hitTestAbove
            , hitTestBelow
            , floorAabb
            , ceilingAabb
            ) where

import Linear.V2

type Rectangle = (V2 Float, V2 Float) -- (x y of top left corner, width height)

data Aabb = Aabb { pMin :: {-# UNPACK #-} !(V2 Float)
                 , pMax :: {-# UNPACK #-} !(V2 Float) }
                 deriving Show

-- see this page for more on Axis aligned bounding boxes 
{- https://developer.mozilla.org/en-US/docs/Games/Techniques/3D_collision_detection -}
hitTest :: Aabb -> Aabb -> Bool
hitTest (Aabb (V2 xmin0 ymin0) (V2 xmax0 ymax0)) (Aabb (V2 xmin1 ymin1) (V2 xmax1 ymax1)) = 
        (xmin0 <= xmax1 && xmax0 >= xmin1) &&
        (ymin0 <= ymax1 && ymax0 >= ymin1)

pointHitTest :: V2 Float -> Aabb -> Bool
pointHitTest (V2 x y) (Aabb (V2 xmin1 ymin1) (V2 xmax1 ymax1)) = 
        (x >= xmin1 && x <= xmax1) &&
        (y >= ymin1 && y <= ymax1)

aabbToRectangle :: Aabb -> Rectangle
aabbToRectangle (Aabb (V2 xmin ymin) (V2 xmax ymax)) = (V2 xmin ymin, V2 (xmax - xmin) (ymax - ymin))

rectangleToAabb :: Rectangle -> Aabb
rectangleToAabb (V2 x y, V2 width height) = Aabb { pMin = V2 x y
                                                 , pMax = V2 (x + width) (y + height) }

{-  hitTestAbove would return true for the following scenario:
    _
   |_| Object
        
   |-----| Line

   returns true because object is above the line
-}
hitTestAbove :: Aabb  -- Object
             -> Aabb -- Line to test if Object is above it. This must have the same y coordinates
             -> Bool
hitTestAbove (Aabb (V2 xmin0 ymin0) (V2 xmax0 ymax0)) (Aabb (V2 xmin1 ymin1) (V2 xmax1 ymax1)) = 
        (xmin0 <= xmax1 && xmax0 >= xmin1) 
        && (ymin0 <= ymin1 || ymax0 <= ymax1 )


{-  hitTestBelow would return true for the following scenario:

   |-----| Line
    _
   |_| Object
        
   returns true because object is above the line
-}
hitTestBelow :: Aabb  -- Object
             -> Aabb -- Line to test if Object is above it. This must have the same y coordinates
             -> Bool
hitTestBelow (Aabb (V2 xmin0 ymin0) (V2 xmax0 ymax0)) (Aabb (V2 xmin1 ymin1) (V2 xmax1 ymax1)) = 
        (xmin0 <= xmax1 && xmax0 >= xmin1) 
        && (ymin0 >= ymin1 || ymax0 >= ymax1 )

{- floorAabb would transform the following Aabb into a line of the lower side: 
    ____         
   |    |        
   |    |  -->   
   |____|        ____
-}
floorAabb :: Aabb -> Aabb
floorAabb (Aabb (V2 xmin ymin) (V2 xmax ymax)) = Aabb (V2 xmin ymax) (V2 xmax ymax)

{- ceilingAabb would transform the following Aabb into a line of the lower side: 
    ____         ____
   |    |        
   |    |  -->   
   |____|       
-}
ceilingAabb :: Aabb -> Aabb
ceilingAabb (Aabb (V2 xmin ymin) (V2 xmax ymax)) = Aabb (V2 xmin ymin) (V2 xmax ymin)
