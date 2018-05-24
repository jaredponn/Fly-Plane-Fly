module Buttons where

import Control.Monad.State.Lazy
import Control.Monad.Reader
import Linear.V2
import SDL (Rectangle (..)
           , Point (..)
           , Texture (..))

import Aabb
import GameVars
import Logger
import Util
import RectangleTransforms

data ButtonAttr = ButtonAttr { rect :: Rectangle Float
                             , aabb :: Aabb
                             , texture :: Texture}

type Button m = ReaderT (ButtonAttr) m ()

createButtonAttrFromAabb :: Aabb -> ButtonAttr
createButtonAttrFromAabb aabb = ButtonAttr { rect = aabbToRectangle aabb
                                            , aabb = aabb }

createButtonAttrFromRectangle :: Rectangle Float -> ButtonAttr
createButtonAttrFromRectangle nrect = ButtonAttr { rect = nrect 
                                         , aabb = rectangleToAabb nrect }

createXCenteredButtonAttr :: (RectangleTransforms m) => Float  -- y position
                          -> V2 Float -- lengths.  Width, height
                          -> m ButtonAttr
createXCenteredButtonAttr ypos lengths = do
        let tmpbtnattr = ButtonAttr { rect = Rectangle (P (V2 0 ypos)) lengths
                                    , aabb = Aabb (V2 0 0) (V2 0 0)}
        xCenterButton tmpbtnattr 

xCenterButton :: (RectangleTransforms m ) => ButtonAttr -> m ButtonAttr  
xCenterButton btnattr = do
        let rectangle = rect btnattr
        rectangle' <- xCenterRectangle rectangle
        return ButtonAttr { rect = rectangle'
                          , aabb = rectangleToAabb rectangle' }

yCenterButton :: (RectangleTransforms m ) => ButtonAttr -> m ButtonAttr  
yCenterButton btnattr = do
        let rectangle = rect btnattr
        rectangle' <- yCenterRectangle rectangle
        return ButtonAttr { rect = rectangle'
                          , aabb = rectangleToAabb rectangle' }
