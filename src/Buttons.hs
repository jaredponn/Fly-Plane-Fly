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

createButtonAttrFromAabb :: Aabb -> Texture -> ButtonAttr
createButtonAttrFromAabb aabb texture = ButtonAttr { rect = aabbToRectangle aabb
                                                   , aabb = aabb 
                                                   , texture = texture}

createButtonAttrFromRectangle :: Rectangle Float -> Texture -> ButtonAttr
createButtonAttrFromRectangle nrect texture = ButtonAttr { rect = nrect 
                                                         , aabb = rectangleToAabb nrect
                                                         , texture = texture}

createXCenteredButtonAttr :: (RectangleTransforms m) => Float  -- y position
                          -> V2 Float -- lengths.  Width, height
                          -> Texture
                          -> m ButtonAttr
createXCenteredButtonAttr ypos lengths texture = do
        let tmpbtnattr = ButtonAttr { rect = Rectangle (P (V2 0 ypos)) lengths
                                    , aabb = Aabb (P (V2 0 0)) (P (V2 0 0))
                                    , texture = undefined}
        xCenterButton tmpbtnattr texture

xCenterButton :: (RectangleTransforms m ) => ButtonAttr -> Texture -> m ButtonAttr  
xCenterButton btnattr texture = do
        let rectangle = rect btnattr
        rectangle' <- xCenterRectangle rectangle
        return ButtonAttr { rect = rectangle'
                          , aabb = rectangleToAabb rectangle'
                          , texture = texture }

yCenterButton :: (RectangleTransforms m ) => ButtonAttr -> Texture -> m ButtonAttr  
yCenterButton btnattr texture = do
        let rectangle = rect btnattr
        rectangle' <- yCenterRectangle rectangle
        return ButtonAttr { rect = rectangle'
                          , aabb = rectangleToAabb rectangle'
                          , texture = texture }
