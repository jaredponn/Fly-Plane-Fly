module ScoreManager where


class (Monad m) => ScoreManager m where
        incrementScore :: m ()
        getScore :: m (Int)
        resetScore :: m ()
