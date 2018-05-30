{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleContexts #-}

module SoundManager where

import qualified SDL.Mixer as Mixer
import Control.Monad.Reader (MonadReader (..)
                            , ReaderT (..)
                            , asks
                            , join)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Lens


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
                jumpfx <- view $ cResources.cSound.jumpFx
                -- it will play the sound on the channel above the music channel
                jumpfxchannel <- views (cResources.cSound.bgMusicChannel) (+1)
                _ <- Mixer.playOn jumpfxchannel Mixer.Once jumpfx
                return ()

        pauseBgMusic :: (MonadIO m, MonadReader Config m) => m ()
        pauseBgMusic = join $ views (cResources.cSound.bgMusicChannel) Mixer.pause

        resumeBgMusic :: (MonadIO m, MonadReader Config m) => m ()
        resumeBgMusic = join $ views (cResources.cSound.bgMusicChannel) Mixer.resume

        setBgMusicVolume ::(MonadIO m, MonadReader Config m) => Mixer.Volume -> m ()
        setBgMusicVolume volume = join $ views (cResources.cSound.bgMusicChannel) (Mixer.setVolume volume)

        getBgMusicVolume :: (MonadIO m, MonadReader Config m) => m (Mixer.Volume)
        getBgMusicVolume = join $ views (cResources.cSound.bgMusicChannel) Mixer.getVolume
