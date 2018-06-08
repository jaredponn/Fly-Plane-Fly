module Aabb ( Aabb (..)
            , hitTest 
            , aabbToRectangle
            , rectangleToAabb
            , pointHitTest
            , hitTestAbove
            , hitTestBelow
            , floorAabb
            , ceilingAabb
            , shiftAabb
            , addCushiontoAabb 
            ) where

import Linear.V2
import SDL (Rectangle (..)
           , Point (..))

data Aabb = Aabb { pMin :: {-# UNPACK #-} !(Point V2 Float)
                 , pMax :: {-# UNPACK #-} !(Point V2 Float) }
                 deriving Show

-- see this page for more on Axis aligned bounding boxes 
{- https://developer.mozilla.org/en-US/docs/Games/Techniques/3D_collision_detection -}

hitTest :: Aabb -> Aabb -> Bool
hitTest (Aabb (P (V2 xmin0 ymin0)) (P (V2 xmax0 ymax0))) (Aabb (P (V2 xmin1 ymin1)) (P (V2 xmax1 ymax1))) = 
        (xmin0 <= xmax1 && xmax0 >= xmin1) &&
        (ymin0 <= ymax1 && ymax0 >= ymin1)

pointHitTest :: Point V2 Float -> Aabb -> Bool
pointHitTest (P (V2 x y)) (Aabb (P (V2 xmin1 ymin1)) (P (V2 xmax1 ymax1))) = 
        (x >= xmin1 && x <= xmax1) &&
        (y >= ymin1 && y <= ymax1)

aabbToRectangle :: Aabb -> Rectangle Float
aabbToRectangle (Aabb (P (V2 xmin ymin)) (P (V2 xmax ymax))) = Rectangle (P (V2 xmin ymin)) (V2 (xmax - xmin) (ymax - ymin))

rectangleToAabb :: Rectangle Float -> Aabb
rectangleToAabb (Rectangle (P (V2 x y)) (V2 width height)) = Aabb { pMin = P $ V2 x y
                                                                  , pMax = P $ V2 (x + width) (y + height) }

{-  hitTestAbove would return true for the following scenario:
    _
   |_| Object
        
   |-----| Line

   returns true because object is above the line
-}
hitTestAbove :: Aabb  -- Object
             -> Aabb -- Line to test if Object is above it. This must have the same y coordinates
             -> Bool
hitTestAbove (Aabb (P (V2 xmin0 ymin0)) (P (V2 xmax0 ymax0))) (Aabb (P (V2 xmin1 ymin1)) (P (V2 xmax1 ymax1))) = 
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
hitTestBelow (Aabb (P (V2 xmin0 ymin0)) (P (V2 xmax0 ymax0))) (Aabb (P (V2 xmin1 ymin1)) (P (V2 xmax1 ymax1))) = 
        (xmin0 <= xmax1 && xmax0 >= xmin1) 
        && (ymin0 >= ymin1 || ymax0 >= ymax1 )

{- floorAabb would transform the following Aabb into a line of the lower side: 
    ____         
   |    |        
   |    |  -->   
   |____|        ____
-}
floorAabb :: Aabb -> Aabb
floorAabb (Aabb (P (V2 xmin _)) (P (V2 xmax ymax))) = Aabb (P (V2 xmin ymax)) (P (V2 xmax ymax))

{- ceilingAabb would transform the following Aabb into a line of the lower side: 
    ____         ____
   |    |        
   |    |  -->   
   |____|       
-}
ceilingAabb :: Aabb -> Aabb
ceilingAabb (Aabb (P (V2 xmin ymin)) (P (V2 xmax _))) = Aabb (P (V2 xmin ymin)) (P (V2 xmax ymin))

-- shifts the aabb by the given (x, y) coordinates
shiftAabb :: V2 Float -> Aabb-> Aabb
shiftAabb  (V2 xshift yshift) (Aabb (P (V2 xmin ymin)) (P (V2 xmax _))) = Aabb (P (V2 (xmin + xshift) (ymin + yshift))) (P (V2 (xmax + xshift) (ymin + yshift)))

{- addCushiontoAabb adds the given amount to all sides of the aabb. For example:
addCushiontoAabb (V2 1 1) _ would result in something like:
    _          __
   |_|  -->   |  |  
              |__|

However, the aabb would not be translated like in this text representation.
If given a negative value, the aabb will shrink in size.
-}
addCushiontoAabb :: V2 Float -> Aabb -> Aabb
addCushiontoAabb (V2 xcushion ycushion) (Aabb (P (V2 xmin ymin)) (P (V2 xmax _))) = Aabb (P (V2 (xmin - xcushion) (ymin - ycushion))) (P (V2 (xmax + xcushion) (ymin + ycushion)))
