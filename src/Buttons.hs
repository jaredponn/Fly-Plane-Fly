{-# LANGUAGE BangPatterns #-}
module Buttons where

import Control.Monad.State.Lazy
import Control.Monad.Reader
import Linear.V2
import SDL 

import Aabb
import GuiTransforms
import GameVars

data ButtonAttr = ButtonAttr { rect :: {-# UNPACK #-} !(Rectangle Float)
                             , aabb :: {-# UNPACK #-} !Aabb
                             , texture :: Texture}

type Button m = ReaderT (ButtonAttr) m ()


-- buttonEffectFromMouse takes an action and the current input and will apply that function if the mouse is over the button and clicking it
buttonEffectFromMouse :: Monad m => m ()  -- action to modify the game
                                 -> Input  -- current input of the game
                                 -> Button m
buttonEffectFromMouse f input = do
        let mousepos = (\(V2 a b) -> (SDL.P (V2 (fromIntegral a) (fromIntegral b)))) . _mousePos $ input
            mousepress = _mousePress input

        btnattr <- ask 

        if pointHitTest mousepos (aabb btnattr) && mousepress
           then lift f
           else return () 

{- FUNCTIONS TO CREATE BUTTONS -}
createButtonAttrFromAabb :: Aabb -> Texture -> ButtonAttr
createButtonAttrFromAabb !naabb !ntexture = ButtonAttr { rect = aabbToRectangle naabb
                                                       , aabb = naabb 
                                                       , texture = ntexture}

createButtonAttrFromRectangle :: Rectangle Float -> Texture -> ButtonAttr
createButtonAttrFromRectangle !nrect !ntexture = ButtonAttr { rect = nrect 
                                                         , aabb = rectangleToAabb nrect
                                                         , texture = ntexture}

createCenteredButtonAttr :: (GuiTransforms m) => V2 Float -- lengths.  Width, height
                          -> Texture
                          -> m ButtonAttr
createCenteredButtonAttr !lengths !ntexture = do
        let tmpbtnattr = ButtonAttr { rect = Rectangle (P (V2 0 0)) lengths
                                    , aabb = Aabb (P (V2 0 0)) (P (V2 0 0))
                                    , texture = ntexture}
        yCenterButtonAttr >=> xCenterButtonAttr $ tmpbtnattr


createXCenteredButtonAttr :: (GuiTransforms m) => Float  -- y position
                          -> V2 Float -- lengths.  Width, height
                          -> Texture
                          -> m ButtonAttr
createXCenteredButtonAttr !ypos !lengths !ntexture = do
        let tmpbtnattr = ButtonAttr { rect = Rectangle (P (V2 0 ypos)) lengths
                                    , aabb = Aabb (P (V2 0 0)) (P (V2 0 0))
                                    , texture = ntexture}
        xCenterButtonAttr tmpbtnattr

createLeftEdgeAlignedButtonAttr :: (GuiTransforms m) => Float  -- y position
                                -> V2 Float -- lengths.  Width, height
                                -> Texture
                                -> m ButtonAttr
createLeftEdgeAlignedButtonAttr !ypos !lengths !ntexture = do
        let tmpbtnattr = ButtonAttr { rect = Rectangle (P (V2 0 ypos)) lengths
                                    , aabb = Aabb (P (V2 0 0)) (P (V2 0 0))
                                    , texture = ntexture}
        alignToLeftEdgeButtonAttr tmpbtnattr

createRightEdgeAlignedButtonAttr :: (GuiTransforms m) => Float  -- y position
                                -> V2 Float -- lengths.  Width, height
                                -> Texture
                                -> m ButtonAttr
createRightEdgeAlignedButtonAttr !ypos !lengths !ntexture = do
        let tmpbtnattr = ButtonAttr { rect = Rectangle (P (V2 0 ypos)) lengths
                                    , aabb = Aabb (P (V2 0 0)) (P (V2 0 0))
                                    , texture = ntexture}
        alignToRightEdgeButtonAttr tmpbtnattr


{- TRANSFORMS TO BUTTON ATTRIBUTES -}
xCenterButtonAttr :: (GuiTransforms m ) => ButtonAttr -> m ButtonAttr  
xCenterButtonAttr !btnattr = do
        let rectangle = rect btnattr
        rectangle' <- xCenterRectangle rectangle
        return btnattr { rect = rectangle'
                       , aabb = rectangleToAabb rectangle' }

yCenterButtonAttr :: (GuiTransforms m ) => ButtonAttr -> m ButtonAttr  
yCenterButtonAttr !btnattr = do
        let rectangle = rect btnattr
        rectangle' <- yCenterRectangle rectangle
        return btnattr { rect = rectangle'
                       , aabb = rectangleToAabb rectangle'}

alignToLeftEdgeButtonAttr :: (GuiTransforms m ) => ButtonAttr -> m ButtonAttr  
alignToLeftEdgeButtonAttr !btnattr = do
        let rectangle = rect btnattr
        rectangle' <- alignToLeftEdge rectangle
        return btnattr { rect = rectangle'
                       , aabb = rectangleToAabb rectangle'}

alignToRightEdgeButtonAttr :: (GuiTransforms m ) => ButtonAttr -> m ButtonAttr  
alignToRightEdgeButtonAttr !btnattr = do
        let rectangle = rect btnattr
        rectangle' <- alignToRightEdge rectangle
        return btnattr { rect = rectangle'
                       , aabb = rectangleToAabb rectangle'}

alignToBottomEdgeButtonAttr :: (GuiTransforms m ) => ButtonAttr -> m ButtonAttr  
alignToBottomEdgeButtonAttr !btnattr = do
        let rectangle = rect btnattr
        rectangle' <- alignToBottomEdge rectangle
        return btnattr { rect = rectangle'
                       , aabb = rectangleToAabb rectangle'}

translateButtonAttr :: V2 Float -> ButtonAttr -> ButtonAttr
translateButtonAttr !ntranslation !btnattr = let nrect = GuiTransforms.translate ntranslation $ rect btnattr
                                              in btnattr { rect = nrect
                                                         , aabb = rectangleToAabb nrect}
