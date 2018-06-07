{-# LANGUAGE InstanceSigs #-} 
{-# LANGUAGE FlexibleContexts #-}

module SoundManager where

import qualified SDL.Mixer as Mixer
import Control.Monad.Reader (MonadReader (..)
                            , join)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Lens


import GameVars

class Monad m => SoundManager m where
        playJumpFx :: m ()
        playCrashFx :: m ()

        muteAll :: m ()
        resumeAll :: m ()
        areChannelsPlaying :: m (Bool)

        setBgMusicVolume :: Mixer.Volume -> m ()
        getBgMusicVolume :: m (Mixer.Volume)


instance SoundManager FlyPlaneFly where
        playJumpFx :: (MonadReader Config m, MonadIO m) => m ()
        playJumpFx = do
                jumpfx <- view $ cResources.cSound.jumpFx
                -- it will play the sound on the channel above the music channel
                jumpfxchannel <- views (cResources.cSound.bgMusicChannel) (+1)
                _ <- Mixer.playOn jumpfxchannel Mixer.Once jumpfx
                return ()

        playCrashFx :: (MonadReader Config m, MonadIO m) => m ()
        playCrashFx = do
                jumpfx <- view $ cResources.cSound.crashFx
                -- it will play the sound on 2 channels above the music channel
                jumpfxchannel <- views (cResources.cSound.bgMusicChannel) (+2)
                _ <- Mixer.playOn jumpfxchannel Mixer.Once jumpfx
                return ()

        muteAll :: (MonadIO m, MonadReader Config m) => m ()
        muteAll = Mixer.setVolume 0 Mixer.AllChannels

        resumeAll :: (MonadIO m, MonadReader Config m) => m ()
        resumeAll = Mixer.setVolume 100 Mixer.AllChannels

        setBgMusicVolume ::(MonadIO m, MonadReader Config m) => Mixer.Volume -> m ()
        setBgMusicVolume volume = join $ views (cResources.cSound.bgMusicChannel) (Mixer.setVolume volume)

        getBgMusicVolume :: (MonadIO m, MonadReader Config m) => m (Mixer.Volume)
        getBgMusicVolume = join $ views (cResources.cSound.bgMusicChannel) Mixer.getVolume

        areChannelsPlaying :: MonadIO m => m (Bool)
        areChannelsPlaying = (>0) <$> Mixer.getVolume Mixer.AllChannels 


pauseOrPlaySounds :: SoundManager m => m ()
pauseOrPlaySounds = do
        arechannelsplaying <- areChannelsPlaying
        if not arechannelsplaying 
           then resumeAll
           else muteAll 
