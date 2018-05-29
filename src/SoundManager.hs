{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleContexts #-}

module SoundManager where

import qualified SDL.Mixer as Mixer
import Control.Monad.Reader (MonadReader (..)
                            , ReaderT (..)
                            , asks)
import Control.Monad.IO.Class (MonadIO(..))


import GameVars

class Monad m => SoundManager m where
        playJumpFx :: m ()

        pauseBgMusic :: m ()
        resumeBgMusic :: m ()

        setBgMusicVolume :: Mixer.Volume -> m ()
        getBgMusicVolume :: m (Mixer.Volume)


instance SoundManager MahppyBird where
        playJumpFx :: (MonadReader Config m, MonadIO m) => m ()
        playJumpFx = do
                resources <- asks cResources
                _ <- Mixer.playOn (bgMusicChannel resources + 1) Mixer.Once $ jumpFx resources
                return ()


        pauseBgMusic :: (MonadIO m, MonadReader Config m) => m ()
        pauseBgMusic = bgMusicChannel <$> asks cResources >>= Mixer.pause

        resumeBgMusic :: (MonadIO m, MonadReader Config m) => m ()
        resumeBgMusic = bgMusicChannel <$> asks cResources >>= Mixer.resume

        setBgMusicVolume ::(MonadIO m, MonadReader Config m) => Mixer.Volume -> m ()
        setBgMusicVolume volume = bgMusicChannel <$> asks cResources >>= Mixer.setVolume volume

        getBgMusicVolume :: (MonadIO m, MonadReader Config m) => m (Mixer.Volume)
        getBgMusicVolume = bgMusicChannel <$> asks cResources >>= Mixer.getVolume 
