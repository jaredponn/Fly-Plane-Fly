{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE FlexibleContexts #-} 
module ButtonTransforms where

import Control.Monad.Reader
import Linear.V2

import Aabb
import GameVars
import Logger
import Util


data Button m = Button { rect :: {-#UNPACK#-} !Rectangle
                       , aabb :: {-#UNPACK#-} !Aabb
                       , effect :: ( Button m -> m () ) }

class Monad m => ButtonTransforms m where
        xCenterButton :: Button m -> m (Button m)
        yCenterButton :: Button m -> m (Button m)


createButtonFromAabb :: (Monad m) => Aabb -> Button m
createButtonFromAabb aabb = Button { rect = aabbToRectangle aabb
                                    , aabb = aabb
                                    , effect = undefined}

createButtonFromRectangle :: Monad m => Rectangle -> Button m
createButtonFromRectangle nrect = Button { rect = nrect 
                                         , aabb = rectangleToAabb nrect
                                         , effect = undefined}


instance ButtonTransforms MahppyBird where
        xCenterButton :: (Logger m, MonadReader Config m) => Button m -> m (Button m)
        xCenterButton button = do
                (winW, winH) <- (\(a, b) -> (fromIntegral a, fromIntegral b)) <$> asks cWindowSize
                let V2 width height = snd . rect $ button
                    V2 _ y = fst . rect $ button
                    topleftpt = V2 (((winW) / 2) - (width / 2)) y
                    nrect = (topleftpt, V2 width height )
                return Button { rect = nrect
                              , aabb = rectangleToAabb nrect
                              , effect = undefined }

        {- yCenterButton :: MonadReader Config m => Button -> m (Button) -}
        {- yCenterButton ((_, V2 width height), (Aabb (V2 x y) _)) = do -}
        {-         (winW, winH) <- asks cWindowSize -}
        {-         let topleftpt = V2 x (((fromIntegral winH) / 2) - (height / 2))  -}
        {-             rect = (topleftpt, V2 width height ) -}
        {-         return (rect, rectangleToAabb rect) -}


        {- xCenterButton ((V2 x y, V2 width height), _) = do -}
        {-         (winW, winH) <- (\(a, b) -> (fromIntegral a, fromIntegral b)) <$> asks cWindowSize -}
        {-         let topleftpt = V2 (((winW) / 2) - (width / 2)) y -}
        {-             rect = (topleftpt, V2 width height ) -}
