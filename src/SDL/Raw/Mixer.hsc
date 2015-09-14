{-|

Module      : SDL.Raw.Mixer
License     : BSD3
Stability   : experimental

Raw bindings to the @SDL2_mixer@ library. No error-handling is done here. For
more information about specific function behaviour, see the @SDL2_mixer@
documentation.

-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module SDL.Raw.Mixer
  (

  -- * General
    getVersion
  , pattern SDL_MIXER_MAJOR_VERSION
  , pattern SDL_MIXER_MINOR_VERSION
  , pattern SDL_MIXER_PATCHLEVEL
  , InitFlag
  , init
  , pattern INIT_FLAC
  , pattern INIT_MOD
  , pattern INIT_MODPLUG
  , pattern INIT_MP3
  , pattern INIT_OGG
  , pattern INIT_FLUIDSYNTH
  , quit
  , Format
  , pattern DEFAULT_FORMAT
  , pattern DEFAULT_FREQUENCY
  , pattern DEFAULT_CHANNELS
  , openAudio
  , pattern AUDIO_U8
  , pattern AUDIO_S8
  , pattern AUDIO_U16LSB
  , pattern AUDIO_S16LSB
  , pattern AUDIO_U16MSB
  , pattern AUDIO_S16MSB
  , pattern AUDIO_U16
  , pattern AUDIO_S16
  , pattern AUDIO_U16SYS
  , pattern AUDIO_S16SYS
  , closeAudio
  , querySpec

  -- * Samples
  , getNumChunkDecoders
  , getChunkDecoder
  , Chunk(..)
  , loadWAV
  , loadWAV_RW
  , quickLoadWAV
  , quickLoadRaw
  , pattern MAX_VOLUME
  , volumeChunk
  , freeChunk

  -- * Channels
  , allocateChannels
  , pattern CHANNELS
  , Channel
  , volume
  , playChannel
  , playChannelTimed
  , fadeInChannel
  , fadeInChannelTimed
  , pause
  , resume
  , haltChannel
  , expireChannel
  , fadeOutChannel
  , channelFinished
  , wrapChannelCallback
  , playing
  , paused
  , Fading
  , fadingChannel
  , pattern NO_FADING
  , pattern FADING_OUT
  , pattern FADING_IN
  , getChunk

  -- * Groups
  , reserveChannels
  , Tag
  , groupChannel
  , groupChannels
  , groupCount
  , groupAvailable
  , groupOldest
  , groupNewer
  , fadeOutGroup
  , haltGroup

  -- * Music
  , getNumMusicDecoders
  , getMusicDecoder
  , Music
  , loadMUS
  , loadMUS_RW
  , loadMUSType_RW
  , freeMusic
  , playMusic
  , fadeInMusic
  , fadeInMusicPos
  , hookMusic
  , volumeMusic
  , pauseMusic
  , resumeMusic
  , rewindMusic
  , setMusicPosition
  , setMusicCMD
  , haltMusic
  , fadeOutMusic
  , wrapMusicCallback
  , hookMusicFinished
  , MusicType
  , getMusicType
  , pattern MUS_NONE
  , pattern MUS_CMD
  , pattern MUS_WAV
  , pattern MUS_MOD
  , pattern MUS_MID
  , pattern MUS_OGG
  , pattern MUS_MP3
  , pattern MUS_MP3_MAD
  , pattern MUS_FLAC
  , pattern MUS_MODPLUG
  , playingMusic
  , pausedMusic
  , fadingMusic
  , getMusicHookData

  -- * Effects
  , Effect
  , wrapEffect
  , EffectFinished
  , wrapEffectFinished
  , registerEffect
  , pattern CHANNEL_POST
  , unregisterEffect
  , unregisterAllEffects
  , setPostMix
  , setPanning
  , setDistance
  , setPosition
  , setReverseStereo

  -- * MikMod
  , setSynchroValue
  , getSynchroValue

  -- * MIDI backends
  , setSoundFonts
  , getSoundFonts
  , eachSoundFont

  ) where

#include "SDL_mixer.h"

import Control.Monad.IO.Class
import Data.Int         (Int16)
import Data.Word        (Word8, Word16, Word32)
import Foreign.C.String (CString)
import Foreign.C.Types  (CInt(..), CDouble(..))
import Foreign.Ptr      (Ptr, FunPtr)
import Foreign.Storable (Storable(..))
import Prelude   hiding (init)
import SDL.Raw.Types    (RWops(..), Version(..))

-- 4.1 General


foreign import ccall safe "static Mix_Linked_Version" getVersion' :: IO (Ptr Version)
{-# INLINE getVersion #-}
getVersion :: forall m_a67p. MonadIO m_a67p => m_a67p (Ptr Version)
getVersion = liftIO getVersion'

pattern SDL_MIXER_MAJOR_VERSION = (#const SDL_MIXER_MAJOR_VERSION)
pattern SDL_MIXER_MINOR_VERSION = (#const SDL_MIXER_MINOR_VERSION)
pattern SDL_MIXER_PATCHLEVEL    = (#const SDL_MIXER_PATCHLEVEL)

type InitFlag = CInt


foreign import ccall safe "static Mix_Init" init' :: InitFlag -> IO CInt
{-# INLINE init #-}
init x_a5Vq = liftIO (init' x_a5Vq)

pattern INIT_FLAC       = (#const MIX_INIT_FLAC)
pattern INIT_MOD        = (#const MIX_INIT_MOD)
pattern INIT_MODPLUG    = (#const MIX_INIT_MODPLUG)
pattern INIT_MP3        = (#const MIX_INIT_MP3)
pattern INIT_OGG        = (#const MIX_INIT_OGG)
pattern INIT_FLUIDSYNTH = (#const MIX_INIT_FLUIDSYNTH)

type Format = Word16


foreign import ccall safe "static Mix_Quit" quit' :: IO ()
{-# INLINE quit #-}
quit :: forall m_a684. MonadIO m_a684 => m_a684 ()
quit = liftIO quit'

pattern DEFAULT_FREQUENCY = (#const MIX_DEFAULT_FREQUENCY)
pattern DEFAULT_CHANNELS  = (#const MIX_DEFAULT_CHANNELS)


foreign import ccall safe "static Mix_OpenAudio" openAudio' :: CInt -> Format -> CInt -> CInt -> IO CInt
{-# INLINE openAudio #-}
openAudio x_a69c x_a69d x_a69e x_a69f = liftIO (openAudio' x_a69c x_a69d x_a69e x_a69f)

pattern AUDIO_U8       = (#const AUDIO_U8)
pattern AUDIO_S8       = (#const AUDIO_S8)
pattern AUDIO_U16LSB   = (#const AUDIO_U16LSB)
pattern AUDIO_S16LSB   = (#const AUDIO_S16LSB)
pattern AUDIO_U16MSB   = (#const AUDIO_U16MSB)
pattern AUDIO_S16MSB   = (#const AUDIO_S16MSB)
pattern AUDIO_U16      = (#const AUDIO_U16)
pattern AUDIO_S16      = (#const AUDIO_S16)
pattern AUDIO_U16SYS   = (#const AUDIO_U16SYS)
pattern AUDIO_S16SYS   = (#const AUDIO_S16SYS)
pattern DEFAULT_FORMAT = (#const MIX_DEFAULT_FORMAT)


foreign import ccall safe "static Mix_CloseAudio" closeAudio' :: IO ()
{-# INLINE closeAudio #-}
closeAudio :: forall m_a69W. MonadIO m_a69W => m_a69W ()
closeAudio = liftIO closeAudio'


foreign import ccall safe "static Mix_QuerySpec" querySpec' :: Ptr CInt -> Ptr Format -> Ptr CInt -> IO CInt
{-# INLINE querySpec #-}
querySpec x_a6bf x_a6bg x_a6bh = liftIO (querySpec' x_a6bf x_a6bg x_a6bh)

-- 4.2 Samples

foreign import ccall safe "static Mix_GetNumChunkDecoders" getNumChunkDecoders' :: IO CInt
{-# INLINE getNumChunkDecoders #-}
getNumChunkDecoders :: forall m_a6c3. MonadIO m_a6c3 => m_a6c3 CInt
getNumChunkDecoders = liftIO getNumChunkDecoders'

foreign import ccall safe "static Mix_GetChunkDecoder" getChunkDecoder' :: CInt -> IO CString
{-# INLINE getChunkDecoder #-}
getChunkDecoder x_a6cQ = liftIO (getChunkDecoder' x_a6cQ)

data Chunk = Chunk
  { chunkAllocated :: CInt
  , chunkAbuf      :: Ptr Word8
  , chunkAlen      :: Word32
  , chunkVolume    :: Word8
  } deriving (Eq, Show)

instance Storable Chunk where
  alignment = sizeOf
  sizeOf _  = (#size Mix_Chunk)

  peek ptr =
    Chunk
      <$> (#peek Mix_Chunk, allocated) ptr
      <*> (#peek Mix_Chunk, abuf)      ptr
      <*> (#peek Mix_Chunk, alen)      ptr
      <*> (#peek Mix_Chunk, volume)    ptr

  poke ptr (Chunk {..}) = do
    (#poke Mix_Chunk, allocated) ptr chunkAllocated
    (#poke Mix_Chunk, abuf)      ptr chunkAbuf
    (#poke Mix_Chunk, alen)      ptr chunkAlen
    (#poke Mix_Chunk, volume)    ptr chunkVolume

foreign import ccall safe "static Mix_LoadWAV_helper" loadWAV' :: CString -> IO (Ptr Chunk)
{-# INLINE loadWAV #-}
loadWAV x_a6dP = liftIO (loadWAV' x_a6dP)

foreign import ccall safe "static Mix_LoadWAV_RW" loadWAV_RW' :: Ptr RWops -> CInt -> IO (Ptr Chunk)
{-# INLINE loadWAV_RW #-}
loadWAV_RW x_a6eX x_a6eY = liftIO (loadWAV_RW' x_a6eX x_a6eY)

foreign import ccall safe "static Mix_QuickLoad_WAV" quickLoadWAV' :: Ptr Word8 -> IO (Ptr Chunk)
{-# INLINE quickLoadWAV #-}
quickLoadWAV x_a6g1 = liftIO (quickLoadWAV' x_a6g1)

foreign import ccall safe "static Mix_QuickLoad_RAW" quickLoadRaw' :: Ptr Word8 -> IO (Ptr Chunk)
{-# INLINE quickLoadRaw #-}
quickLoadRaw x_a6h3 = liftIO (quickLoadRaw' x_a6h3)

pattern MAX_VOLUME = (#const MIX_MAX_VOLUME)

foreign import ccall safe "static Mix_VolumeChunk" volumeChunk' :: Ptr Chunk -> CInt -> IO CInt
{-# INLINE volumeChunk #-}
volumeChunk x_a6i6 x_a6i7 = liftIO (volumeChunk' x_a6i6 x_a6i7)

foreign import ccall safe "static Mix_FreeChunk" freeChunk' :: Ptr Chunk -> IO ()
{-# INLINE freeChunk #-}
freeChunk x_a6j0 = liftIO (freeChunk' x_a6j0)

-- 4.3 Channels

foreign import ccall safe "static Mix_AllocateChannels" allocateChannels' :: CInt -> IO CInt
{-# INLINE allocateChannels #-}
allocateChannels x_a6jP = liftIO (allocateChannels' x_a6jP)

pattern CHANNELS = (#const MIX_CHANNELS)

type Channel = CInt

foreign import ccall safe "static Mix_Volume" volume' :: Channel -> CInt -> IO CInt
{-# INLINE volume #-}
volume x_a6kK x_a6kL = liftIO (volume' x_a6kK x_a6kL)

foreign import ccall safe "static Mix_PlayChannel_helper" playChannel' :: Channel -> Ptr Chunk -> CInt -> IO CInt
{-# INLINE playChannel #-}
playChannel x_a6lU x_a6lV x_a6lW = liftIO (playChannel' x_a6lU x_a6lV x_a6lW)

foreign import ccall safe "static Mix_PlayChannelTimed" playChannelTimed' :: Channel -> Ptr Chunk -> CInt -> CInt -> IO CInt
{-# INLINE playChannelTimed #-}
playChannelTimed x_a6ne x_a6nf x_a6ng x_a6nh = liftIO (playChannelTimed' x_a6ne x_a6nf x_a6ng x_a6nh)

foreign import ccall safe "static Mix_FadeInChannel_helper" fadeInChannel' :: Channel -> Ptr Chunk -> CInt -> CInt -> IO CInt
{-# INLINE fadeInChannel #-}
fadeInChannel x_a6oA x_a6oB x_a6oC x_a6oD = liftIO (fadeInChannel' x_a6oA x_a6oB x_a6oC x_a6oD)

foreign import ccall safe "static Mix_FadeInChannelTimed" fadeInChannelTimed' :: Channel -> Ptr Chunk -> CInt -> CInt -> CInt -> IO CInt
{-# INLINE fadeInChannelTimed #-}
fadeInChannelTimed x_a6q3 x_a6q4 x_a6q5 x_a6q6 x_a6q7 = liftIO (fadeInChannelTimed' x_a6q3 x_a6q4 x_a6q5 x_a6q6 x_a6q7)

foreign import ccall safe "static Mix_Pause" pause' :: Channel -> IO ()
{-# INLINE pause #-}
pause x_a6qX = liftIO (pause' x_a6qX)

foreign import ccall safe "static Mix_Resume" resume' :: Channel -> IO ()
{-# INLINE resume #-}
resume x_a6rI = liftIO (resume' x_a6rI)

foreign import ccall safe "static Mix_HaltChannel" haltChannel' :: Channel -> IO CInt
{-# INLINE haltChannel #-}
haltChannel x_a6sw = liftIO (haltChannel' x_a6sw)

foreign import ccall safe "static Mix_ExpireChannel" expireChannel' :: Channel -> CInt -> IO CInt
{-# INLINE expireChannel #-}
expireChannel x_a6tr x_a6ts = liftIO (expireChannel' x_a6tr x_a6ts)

foreign import ccall safe "static Mix_FadeOutChannel" fadeOutChannel' :: Channel -> CInt -> IO CInt
{-# INLINE fadeOutChannel #-}
fadeOutChannel x_a6uo x_a6up = liftIO (fadeOutChannel' x_a6uo x_a6up)

foreign import ccall "wrapper"
  wrapChannelCallback :: (Channel -> IO ()) -> IO (FunPtr (Channel -> IO ()))

foreign import ccall safe "static Mix_ChannelFinished" channelFinished' :: FunPtr (Channel -> IO ()) -> IO ()
{-# INLINE channelFinished #-}
channelFinished x_a6vr = liftIO (channelFinished' x_a6vr)

foreign import ccall safe "static Mix_Playing" playing' :: Channel -> IO CInt
{-# INLINE playing #-}
playing x_a6wm = liftIO (playing' x_a6wm)

foreign import ccall safe "static Mix_Paused" paused' :: Channel -> IO CInt
{-# INLINE paused #-}
paused x_a6xa = liftIO (paused' x_a6xa)

type Fading = (#type Mix_Fading)

pattern NO_FADING  = (#const MIX_NO_FADING)
pattern FADING_IN  = (#const MIX_FADING_IN)
pattern FADING_OUT = (#const MIX_FADING_OUT)

foreign import ccall safe "static Mix_FadingChannel" fadingChannel' :: Channel -> IO Fading
{-# INLINE fadingChannel #-}
fadingChannel x_a6xY = liftIO (fadingChannel' x_a6xY)

foreign import ccall safe "static Mix_GetChunk" getChunk' :: Channel -> IO (Ptr Chunk)
{-# INLINE getChunk #-}
getChunk x_a6yS = liftIO (getChunk' x_a6yS)

-- 4.4 Groups

foreign import ccall safe "static Mix_ReserveChannels" reserveChannels' :: CInt -> IO CInt
{-# INLINE reserveChannels #-}
reserveChannels x_a6zH = liftIO (reserveChannels' x_a6zH)

type Tag = CInt

foreign import ccall safe "static Mix_GroupChannel" groupChannel' :: Channel -> Tag -> IO CInt
{-# INLINE groupChannel #-}
groupChannel x_a6AC x_a6AD = liftIO (groupChannel' x_a6AC x_a6AD)

foreign import ccall safe "static Mix_GroupChannels" groupChannels' :: Channel -> Channel -> Tag -> IO CInt
{-# INLINE groupChannels #-}
groupChannels x_a6BG x_a6BH x_a6BI = liftIO (groupChannels' x_a6BG x_a6BH x_a6BI)

foreign import ccall safe "static Mix_GroupCount" groupCount' :: Tag -> IO CInt
{-# INLINE groupCount #-}
groupCount x_a6Cy = liftIO (groupCount' x_a6Cy)

foreign import ccall safe "static Mix_GroupAvailable" groupAvailable' :: Tag -> IO CInt
{-# INLINE groupAvailable #-}
groupAvailable x_a6Dm = liftIO (groupAvailable' x_a6Dm)

foreign import ccall safe "static Mix_GroupOldest" groupOldest' :: Tag -> IO CInt
{-# INLINE groupOldest #-}
groupOldest x_a6Ea = liftIO (groupOldest' x_a6Ea)

foreign import ccall safe "static Mix_GroupNewer" groupNewer' :: Tag -> IO CInt
{-# INLINE groupNewer #-}
groupNewer x_a6EY = liftIO (groupNewer' x_a6EY)

foreign import ccall safe "static Mix_FadeOutGroup" fadeOutGroup' :: Tag -> CInt -> IO CInt
{-# INLINE fadeOutGroup #-}
fadeOutGroup x_a6FT x_a6FU = liftIO (fadeOutGroup' x_a6FT x_a6FU)

foreign import ccall safe "static Mix_HaltGroup" haltGroup' :: Tag -> IO CInt
{-# INLINE haltGroup #-}
haltGroup x_a6GJ = liftIO (haltGroup' x_a6GJ)

-- 4.5 Music

foreign import ccall safe "static Mix_GetNumMusicDecoders" getNumMusicDecoders' :: IO CInt
{-# INLINE getNumMusicDecoders #-}
getNumMusicDecoders :: forall m_a6Hq. MonadIO m_a6Hq => m_a6Hq CInt
getNumMusicDecoders = liftIO getNumMusicDecoders'

foreign import ccall safe "static Mix_GetMusicDecoder" getMusicDecoder' :: CInt -> IO CString
{-# INLINE getMusicDecoder #-}
getMusicDecoder x_a6Id = liftIO (getMusicDecoder' x_a6Id)

data Music

foreign import ccall safe "static Mix_LoadMUS" loadMUS' :: CString -> IO (Ptr Music)
{-# INLINE loadMUS #-}
loadMUS x_a6J7 = liftIO (loadMUS' x_a6J7)

foreign import ccall safe "static Mix_LoadMUS_RW" loadMUS_RW' :: Ptr RWops -> CInt -> IO (Ptr Music)
{-# INLINE loadMUS_RW #-}
loadMUS_RW x_a6Kf x_a6Kg = liftIO (loadMUS_RW' x_a6Kf x_a6Kg)

type MusicType = (#type Mix_MusicType)

foreign import ccall safe "static Mix_LoadMUSType_RW" loadMUSType_RW' :: Ptr RWops -> MusicType -> CInt -> IO (Ptr Music)
{-# INLINE loadMUSType_RW #-}
loadMUSType_RW x_a6Lx x_a6Ly x_a6Lz = liftIO (loadMUSType_RW' x_a6Lx x_a6Ly x_a6Lz)

pattern MUS_NONE    = (#const MUS_NONE)
pattern MUS_CMD     = (#const MUS_CMD)
pattern MUS_WAV     = (#const MUS_WAV)
pattern MUS_MOD     = (#const MUS_MOD)
pattern MUS_MID     = (#const MUS_MID)
pattern MUS_OGG     = (#const MUS_OGG)
pattern MUS_MP3     = (#const MUS_MP3)
pattern MUS_MP3_MAD = (#const MUS_MP3_MAD)
pattern MUS_FLAC    = (#const MUS_FLAC)
pattern MUS_MODPLUG = (#const MUS_MODPLUG)

foreign import ccall safe "static Mix_FreeMusic" freeMusic' :: Ptr Music -> IO ()
{-# INLINE freeMusic #-}
freeMusic x_a6Mu = liftIO (freeMusic' x_a6Mu)

foreign import ccall safe "static Mix_PlayMusic" playMusic' :: Ptr Music -> CInt -> IO CInt
{-# INLINE playMusic #-}
playMusic x_a6Nw x_a6Nx = liftIO (playMusic' x_a6Nw x_a6Nx)

foreign import ccall safe "static Mix_FadeInMusic" fadeInMusic' :: Ptr Music -> CInt -> CInt -> IO CInt
{-# INLINE fadeInMusic #-}
fadeInMusic x_a6OH x_a6OI x_a6OJ = liftIO (fadeInMusic' x_a6OH x_a6OI x_a6OJ)

foreign import ccall safe "static Mix_FadeInMusicPos" fadeInMusicPos' :: Ptr Music -> CInt -> CInt -> CDouble -> IO CInt
{-# INLINE fadeInMusicPos #-}
fadeInMusicPos x_a6Q1 x_a6Q2 x_a6Q3 x_a6Q4 = liftIO (fadeInMusicPos' x_a6Q1 x_a6Q2 x_a6Q3 x_a6Q4)

foreign import ccall safe "static Mix_HookMusic" hookMusic' :: FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO ()) -> Ptr () -> IO ()
{-# INLINE hookMusic #-}
hookMusic x_a6RK x_a6RL = liftIO (hookMusic' x_a6RK x_a6RL)

foreign import ccall safe "static Mix_VolumeMusic" volumeMusic' :: CInt -> IO CInt
{-# INLINE volumeMusic #-}
volumeMusic x_a6SF = liftIO (volumeMusic' x_a6SF)

foreign import ccall safe "static Mix_PauseMusic" pauseMusic' :: IO ()
{-# INLINE pauseMusic #-}
pauseMusic :: forall m_a6Tj. MonadIO m_a6Tj => m_a6Tj ()
pauseMusic = liftIO pauseMusic'

foreign import ccall safe "static Mix_ResumeMusic" resumeMusic' :: IO ()
{-# INLINE resumeMusic #-}
resumeMusic :: forall m_a6TW. MonadIO m_a6TW => m_a6TW ()
resumeMusic = liftIO resumeMusic'

foreign import ccall safe "static Mix_RewindMusic" rewindMusic' :: IO ()
{-# INLINE rewindMusic #-}
rewindMusic :: forall m_a6Uz. MonadIO m_a6Uz => m_a6Uz ()
rewindMusic = liftIO rewindMusic'

foreign import ccall safe "static Mix_SetMusicPosition" setMusicPosition' :: CDouble -> IO CInt
{-# INLINE setMusicPosition #-}
setMusicPosition x_a6Vm = liftIO (setMusicPosition' x_a6Vm)

foreign import ccall safe "static Mix_SetMusicCMD" setMusicCMD' :: CString -> IO CInt
{-# INLINE setMusicCMD #-}
setMusicCMD x_a6Wa = liftIO (setMusicCMD' x_a6Wa)

foreign import ccall safe "static Mix_HaltMusic" haltMusic' :: IO CInt
{-# INLINE haltMusic #-}
haltMusic :: forall m_a6WR. MonadIO m_a6WR => m_a6WR CInt
haltMusic = liftIO haltMusic'

foreign import ccall safe "static Mix_FadeOutMusic" fadeOutMusic' :: CInt -> IO CInt
{-# INLINE fadeOutMusic #-}
fadeOutMusic x_a6XE = liftIO (fadeOutMusic' x_a6XE)

foreign import ccall "wrapper"
  wrapMusicCallback :: IO () -> IO (FunPtr (IO ()))

foreign import ccall safe "static Mix_HookMusicFinished" hookMusicFinished' :: FunPtr (IO ()) -> IO ()
{-# INLINE hookMusicFinished #-}
hookMusicFinished x_a6Yy = liftIO (hookMusicFinished' x_a6Yy)

foreign import ccall safe "static Mix_GetMusicType" getMusicType' :: Ptr Music -> IO MusicType
{-# INLINE getMusicType #-}
getMusicType x_a6Zu = liftIO (getMusicType' x_a6Zu)

foreign import ccall safe "static Mix_PlayingMusic" playingMusic' :: IO CInt
{-# INLINE playingMusic #-}
playingMusic :: forall m_a70c. MonadIO m_a70c => m_a70c CInt
playingMusic = liftIO playingMusic'

foreign import ccall safe "static Mix_PausedMusic" pausedMusic' :: IO CInt
{-# INLINE pausedMusic #-}
pausedMusic :: forall m_a70S. MonadIO m_a70S => m_a70S CInt
pausedMusic = liftIO pausedMusic'

foreign import ccall safe "static Mix_FadingChannel" fadingMusic' :: IO Fading
{-# INLINE fadingMusic #-}
fadingMusic :: forall m_a71y. MonadIO m_a71y => m_a71y Fading
fadingMusic = liftIO fadingMusic'

foreign import ccall safe "static Mix_GetMusicHookData" getMusicHookData' :: IO (Ptr ())
{-# INLINE getMusicHookData #-}
getMusicHookData :: forall m_a72h. MonadIO m_a72h => m_a72h (Ptr ())
getMusicHookData = liftIO getMusicHookData'

-- 4.6 Effects

pattern CHANNEL_POST = (#const MIX_CHANNEL_POST)

type Effect = Channel -> Ptr () -> CInt -> Ptr() -> IO ()

foreign import ccall "wrapper"
  wrapEffect :: Effect -> IO (FunPtr Effect)

type EffectFinished = Channel -> Ptr () -> IO ()

foreign import ccall "wrapper"
  wrapEffectFinished :: EffectFinished -> IO (FunPtr EffectFinished)

foreign import ccall safe "static Mix_RegisterEffect" registerEffect' :: Channel -> FunPtr Effect -> FunPtr EffectFinished -> Ptr () -> IO CInt
{-# INLINE registerEffect #-}
registerEffect x_a73G x_a73H x_a73I x_a73J = liftIO (registerEffect' x_a73G x_a73H x_a73I x_a73J)

foreign import ccall safe "static Mix_UnregisterEffect" unregisterEffect' :: Channel -> FunPtr Effect -> IO CInt
{-# INLINE unregisterEffect #-}
unregisterEffect x_a74Q x_a74R = liftIO (unregisterEffect' x_a74Q x_a74R)

foreign import ccall safe "static Mix_UnregisterAllEffects" unregisterAllEffects' :: Channel -> IO CInt
{-# INLINE unregisterAllEffects #-}
unregisterAllEffects x_a75H = liftIO (unregisterAllEffects' x_a75H)

foreign import ccall safe "static Mix_SetPostMix" setPostMix' :: FunPtr (Ptr () -> Ptr Word8 -> CInt -> IO ()) -> Ptr () -> IO ()
{-# INLINE setPostMix #-}
setPostMix x_a77f x_a77g = liftIO (setPostMix' x_a77f x_a77g)

foreign import ccall safe "static Mix_SetPanning" setPanning' :: Channel -> Word8 -> Word8 -> IO CInt
{-# INLINE setPanning #-}
setPanning x_a78o x_a78p x_a78q = liftIO (setPanning' x_a78o x_a78p x_a78q)

foreign import ccall safe "static Mix_SetDistance" setDistance' :: Channel -> Word8 -> IO CInt
{-# INLINE setDistance #-}
setDistance x_a79n x_a79o = liftIO (setDistance' x_a79n x_a79o)

foreign import ccall safe "static Mix_SetPosition" setPosition' :: Channel -> Int16 -> Word8 -> IO CInt
{-# INLINE setPosition #-}
setPosition x_a7ar x_a7as x_a7at = liftIO (setPosition' x_a7ar x_a7as x_a7at)

foreign import ccall safe "static Mix_SetReverseStereo" setReverseStereo' :: Channel -> CInt -> IO CInt
{-# INLINE setReverseStereo #-}
setReverseStereo x_a7bu x_a7bv = liftIO (setReverseStereo' x_a7bu x_a7bv)

-- ?.? Not documented

foreign import ccall safe "static Mix_SetSynchroValue" setSynchroValue' :: CInt -> IO CInt
{-# INLINE setSynchroValue #-}
setSynchroValue x_a7ck = liftIO (setSynchroValue' x_a7ck)

foreign import ccall safe "static Mix_GetSynchroValue" getSynchroValue' :: IO CInt
{-# INLINE getSynchroValue #-}
getSynchroValue :: forall m_a7d1. MonadIO m_a7d1 => m_a7d1 CInt
getSynchroValue = liftIO getSynchroValue'

foreign import ccall safe "static Mix_SetSoundFonts" setSoundFonts' :: Ptr CString -> IO CInt
{-# INLINE setSoundFonts #-}
setSoundFonts x_a7dU = liftIO (setSoundFonts' x_a7dU)

foreign import ccall safe "static Mix_GetSoundFonts" getSoundFonts' :: IO (Ptr CString)
{-# INLINE getSoundFonts #-}
getSoundFonts :: forall m_a7eI. MonadIO m_a7eI => m_a7eI (Ptr CString)
getSoundFonts = liftIO getSoundFonts'

foreign import ccall safe "static Mix_EachSoundFont" eachSoundFont' :: FunPtr (CString -> Ptr () -> IO CInt) -> Ptr () -> IO CInt
{-# INLINE eachSoundFont #-}
eachSoundFont x_a7ga x_a7gb = liftIO (eachSoundFont' x_a7ga x_a7gb)
