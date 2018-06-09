{-# LANGUAGE BangPatterns #-}
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
                  , replaceAnimation
                  , removeAnimations
                  ) where

import qualified Data.Stream as S
import Foreign.C.Types
import SDL

{- Animations are modelled as an infinite streams of SDL.Rectangles that corrospond to sprite sheets, with a data describing what the animation is for. -}

data AnimationHandler = AnimationHandler { srcRectStream :: S.Stream AnimationSrcRect -- stream of the animation
                                         , frameDuration :: !Float  -- duration of each sprite visual
                                         , accTime :: !Float } -- amount of time the current animation is being shown.
                                         deriving Show

data AnimationSrcRect = AnimationSrcRect { srcRect :: Rectangle CInt -- Rectangle of where the animation is on the sprite sheet
                                         , animationType :: AnimationType }  -- the animation type for high level game logic.
                                         deriving Show

data AnimationType = AnimationType'Idle 
                   | AnimationType'Death
                   | AnimationType'Jump
                   deriving (Eq, Show)

createAnimationHandler :: S.Stream AnimationSrcRect -- srcRectStream
                -> Float -- frameDuration
                -> AnimationHandler
createAnimationHandler stream !frameduration = AnimationHandler { srcRectStream = stream
                                                                , frameDuration = frameduration
                                                                , accTime = 0 }

generateSrcRectStream :: [AnimationSrcRect] -- Rectangles of the sprite animation on the sprite sheet
                      -> S.Stream AnimationSrcRect -- infinite stream of specified sprite animation
generateSrcRectStream = S.cycle 

-- generateSrcRects is a helper function to create the array of rectangles corrosponding to the animation on the sprite sheet
generateSrcRects :: Point V2 CInt -- initial starting position
                 -> V2 CInt -- lengths of the sprite
                 -> V2 CInt -- stride. Distance to travel to go to the next sprite animation
                 -> CInt -- Number of sprites
                 -> AnimationType -- animation type
                 -> [AnimationSrcRect]
generateSrcRects !(P (V2 initx inity)) !(V2 width height) !(V2 xstride ystride) !numsprites !animationtype = map f $ [0..(numsprites - 1)]
        where
                f :: CInt -> AnimationSrcRect 
                f n = AnimationSrcRect { srcRect = Rectangle (P ((V2 initx inity) + (V2 (xstride*n) (ystride*n))) ) (V2 width height)
                                       , animationType = animationtype }

-- addTimeToAnimationHandler adds time to the animation handler, so the animation handler knows how long the
-- current sprite has been displayed
addTimeToAnimationHandler :: AnimationHandler -> Float -> AnimationHandler
addTimeToAnimationHandler !animation !t = animation { accTime = t + accTime animation }

-- updateAnimationHandler updates the animation handler so if the current sprite has been shown enough,
-- as determined by the "frameDuration," it will "pop" that animation so the next one will be displayed
updateAnimationHandler :: AnimationHandler -> AnimationHandler
updateAnimationHandler !animation = if accTime animation >= frameDuration animation
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

-- prefixes animations in the stream
prefixAnimation :: [AnimationSrcRect] -> AnimationHandler -> AnimationHandler
prefixAnimation !newanimations animation = animation { srcRectStream = prefix newanimations $ srcRectStream animation  }

-- replaces all animations in the stream
replaceAnimation :: [AnimationSrcRect] -> AnimationHandler -> AnimationHandler
replaceAnimation !newanimations animation = animation { srcRectStream = prefix (cycle newanimations) $ srcRectStream animation  }

-- removes all animations with that type. WARNING can diverge.
removeAnimations :: AnimationType -> AnimationHandler -> AnimationHandler
removeAnimations !animationtype animation = animation { srcRectStream = Animations.dropWhile (==animationtype) $ srcRectStream animation }

-- removes all animations up to a specified animation. WARNING can diverge.
removeAnimationsUpto :: AnimationType -> AnimationHandler -> AnimationHandler
removeAnimationsUpto !animationtype animation = animation { srcRectStream = Animations.dropWhile (/=animationtype) $ srcRectStream animation }

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

