module Gravity where


class Monad m => HasGravity m where
        applyGrav :: m ()
        addVel :: Float -> m ()
        setVel :: Float -> m ()
        applyVel :: m ()
