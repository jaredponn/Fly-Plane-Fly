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
import GuiTransforms

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

createCenteredButtonAttr :: (GuiTransforms m) => V2 Float -- lengths.  Width, height
                          -> Texture
                          -> m ButtonAttr
createCenteredButtonAttr lengths texture = do
        let tmpbtnattr = ButtonAttr { rect = Rectangle (P (V2 0 0)) lengths
                                    , aabb = Aabb (P (V2 0 0)) (P (V2 0 0))
                                    , texture = texture}
        yCenterButtonAttr >=> xCenterButtonAttr $ tmpbtnattr


createXCenteredButtonAttr :: (GuiTransforms m) => Float  -- y position
                          -> V2 Float -- lengths.  Width, height
                          -> Texture
                          -> m ButtonAttr
createXCenteredButtonAttr ypos lengths texture = do
        let tmpbtnattr = ButtonAttr { rect = Rectangle (P (V2 0 ypos)) lengths
                                    , aabb = Aabb (P (V2 0 0)) (P (V2 0 0))
                                    , texture = texture}
        xCenterButtonAttr tmpbtnattr

createLeftEdgeAlignedButtonAttr :: (GuiTransforms m) => Float  -- y position
                                -> V2 Float -- lengths.  Width, height
                                -> Texture
                                -> m ButtonAttr
createLeftEdgeAlignedButtonAttr ypos lengths texture = do
        let tmpbtnattr = ButtonAttr { rect = Rectangle (P (V2 0 ypos)) lengths
                                    , aabb = Aabb (P (V2 0 0)) (P (V2 0 0))
                                    , texture = texture}
        alignToLeftEdgeButtonAttr tmpbtnattr

createRightEdgeAlignedButtonAttr :: (GuiTransforms m) => Float  -- y position
                                -> V2 Float -- lengths.  Width, height
                                -> Texture
                                -> m ButtonAttr
createRightEdgeAlignedButtonAttr ypos lengths texture = do
        let tmpbtnattr = ButtonAttr { rect = Rectangle (P (V2 0 ypos)) lengths
                                    , aabb = Aabb (P (V2 0 0)) (P (V2 0 0))
                                    , texture = texture}
        alignToRightEdgeButtonAttr tmpbtnattr

{- TRANSFORMS TO BUTTON ATTRIBUTES -}
xCenterButtonAttr :: (GuiTransforms m ) => ButtonAttr -> m ButtonAttr  
xCenterButtonAttr btnattr = do
        let rectangle = rect btnattr
        rectangle' <- xCenterRectangle rectangle
        return btnattr { rect = rectangle'
                       , aabb = rectangleToAabb rectangle' }

yCenterButtonAttr :: (GuiTransforms m ) => ButtonAttr -> m ButtonAttr  
yCenterButtonAttr btnattr = do
        let rectangle = rect btnattr
        rectangle' <- yCenterRectangle rectangle
        return btnattr { rect = rectangle'
                       , aabb = rectangleToAabb rectangle'}

alignToLeftEdgeButtonAttr :: (GuiTransforms m ) => ButtonAttr -> m ButtonAttr  
alignToLeftEdgeButtonAttr btnattr = do
        let rectangle = rect btnattr
        rectangle' <- alignToLeftEdge rectangle
        return btnattr { rect = rectangle'
                       , aabb = rectangleToAabb rectangle'}

alignToRightEdgeButtonAttr :: (GuiTransforms m ) => ButtonAttr -> m ButtonAttr  
alignToRightEdgeButtonAttr btnattr = do
        let rectangle = rect btnattr
        rectangle' <- alignToRightEdge rectangle
        return btnattr { rect = rectangle'
                       , aabb = rectangleToAabb rectangle'}

translateButtonAttr :: V2 Float -> ButtonAttr -> ButtonAttr
translateButtonAttr translation btnattr = let nrect = GuiTransforms.translate translation $ rect btnattr
                                           in btnattr { rect = nrect
                                                      , aabb = rectangleToAabb nrect}
