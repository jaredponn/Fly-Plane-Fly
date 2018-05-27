module Animations ( AnimationHandler (..)
                  , AnimationSrcRect (..)
                  , AnimationType (..)
                  , createAnimationHandler
                  , generateSrcRects
                  , generateSrcRectStream
                  , addTimeToAnimationHandler
                  , updateAnimationHandler
                  , headAnimation
                  , prefixAnimation
                  , removeAnimationsUpto
                  , removeAnimations
                  ) where

import qualified Data.Stream as S
import Foreign.C.Types
import SDL

data AnimationHandler = AnimationHandler { srcRectStream :: S.Stream AnimationSrcRect -- stream of the default animation
                           , frameDuration :: Float  -- duration of each sprite visual
                           , accTime :: Float }

data AnimationSrcRect = AnimationSrcRect { srcRect :: Rectangle CInt
                                         , animationType :: AnimationType}

data AnimationType = AnimationType'Idle 
                   | AnimationType'Jump
                   deriving Eq

createAnimationHandler :: Texture -- sprites
                -> S.Stream AnimationSrcRect -- srcRectStream
                -> Float -- frameDuration
                -> AnimationHandler
createAnimationHandler texture stream frameduration = AnimationHandler { srcRectStream = stream
                                                                       , frameDuration = frameduration
                                                                       , accTime = 0 }

generateSrcRectStream :: [AnimationSrcRect]
                      -> S.Stream AnimationSrcRect
generateSrcRectStream = S.cycle 

generateSrcRects :: Point V2 CInt -- initial idle starting position
                 -> V2 CInt -- lengths         
                 -> V2 CInt -- stride
                 -> CInt -- Number of sprites
                 -> AnimationType
                 -> [AnimationSrcRect]
generateSrcRects (P (V2 initx inity)) (V2 width height) (V2 xstride ystride) numsprites animationtype = map f $ [0..(numsprites - 1)]
        where
                f :: CInt -> AnimationSrcRect 
                f n = AnimationSrcRect { srcRect = Rectangle (P ((V2 initx inity) + (V2 (xstride*n) (ystride*n))) ) (V2 width height)
                                       , animationType = animationtype }

addTimeToAnimationHandler :: AnimationHandler -> Float -> AnimationHandler
addTimeToAnimationHandler animation t = animation { accTime = t + accTime animation }

updateAnimationHandler :: AnimationHandler -> AnimationHandler
updateAnimationHandler animation = if accTime animation >= frameDuration animation
                                     then (popAnimation_ animation) { accTime = 0 }
                                     else animation

{- SPECIALIZED FUNCTIONS  -}
headAnimation :: AnimationHandler -> AnimationSrcRect
headAnimation animation = Animations.head . srcRectStream $ animation

popAnimation :: AnimationHandler -> (AnimationSrcRect, AnimationHandler)
popAnimation animation = let (headanimation, tailanimations) = pop . srcRectStream $ animation
                          in (headanimation, animation { srcRectStream = tailanimations })

popAnimation_ :: AnimationHandler -> AnimationHandler
popAnimation_ animation = animation { srcRectStream = pop_ . srcRectStream $ animation  }

prefixAnimation :: [AnimationSrcRect] -> AnimationHandler -> AnimationHandler
prefixAnimation newanimations animation = animation { srcRectStream = prefix newanimations $ srcRectStream animation  }

removeAnimations :: AnimationType -> AnimationHandler -> AnimationHandler
removeAnimations animationtype animation = animation { srcRectStream = Animations.dropWhile (==animationtype) $ srcRectStream animation }

removeAnimationsUpto :: AnimationType -> AnimationHandler -> AnimationHandler
removeAnimationsUpto animationtype animation = animation { srcRectStream = Animations.dropWhile (/=animationtype) $ srcRectStream animation }

{- STANDARD FUNCTIONS  -}
head :: S.Stream AnimationSrcRect -> AnimationSrcRect
head = S.head 

pop :: S.Stream AnimationSrcRect -> (AnimationSrcRect, S.Stream AnimationSrcRect)
pop stream = (S.head stream, S.tail stream)

pop_ :: S.Stream AnimationSrcRect -> S.Stream AnimationSrcRect
pop_ = S.tail

prefix :: [AnimationSrcRect] -> S.Stream AnimationSrcRect -> S.Stream AnimationSrcRect
prefix = S.prefix 

dropWhile :: (AnimationType -> Bool) -> S.Stream AnimationSrcRect -> S.Stream AnimationSrcRect
dropWhile f stream 
  | f (animationType . S.head $ stream) = Animations.dropWhile f (S.tail stream)
  | otherwise = stream

