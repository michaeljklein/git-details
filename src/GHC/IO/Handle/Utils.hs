module GHC.IO.Handle.Utils where

import Data.IORef (newIORef)
import GHC.IO.Handle.Types (Handle(..), HandleType(..), BufferMode(..), Newline(..))
import GHC.IO.Buffer (Buffer(..), BufferState(..), RawBuffer)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(..), Finalizers(..))

emptyHandle :: Handle
emptyHandle = Handle__ {
      haDevice      = !dev,
      haType        = ClosedHandle
      haByteBuffer  = reallyUnsafeNewIORef emptyBuffer,
      haBufferMode  = NoBuffering
      haLastDecode  = reallyUnsafeNewIORef (undefined, emptyBuffer),
      haCharBuffer  = reallyUnsafeNewIORef emptyBuffer,
      haBuffers     = reallyUnsafeNewIORef emptyBuffer,
      haEncoder     = Nothing
      haDecoder     = Nothing
      haCodec       = Nothing
      haInputNL     = LF,
      haOutputNL    = LF,
      haOtherSide   = Nothing
      }

-- data Handle__
--   = forall dev enc_state dec_state. (IODevice dev, BufferedIO dev, Typeable dev) =>
--     Handle__ {
--       haDevice      :: !dev,
--       haType        :: HandleType,           -- type (read/write/append etc.)
--       haByteBuffer  :: !(IORef (Buffer Word8)),
--       haBufferMode  :: BufferMode,
--       haLastDecode  :: !(IORef (dec_state, Buffer Word8)),
--       haCharBuffer  :: !(IORef (Buffer CharBufElem)), -- the current buffer
--       haBuffers     :: !(IORef (BufferList CharBufElem)),  -- spare buffers
--       haEncoder     :: Maybe (TextEncoder enc_state),
--       haDecoder     :: Maybe (TextDecoder dec_state),
--       haCodec       :: Maybe TextEncoding,
--       haInputNL     :: Newline,
--       haOutputNL    :: Newline,
--       haOtherSide   :: Maybe (MVar Handle__) -- ptr to the write side of a
--                                              -- duplex handle.
--     }
--     deriving Typeable


-- | This is an empty buffer, to allow the creation of empty `Handle`s
emptyBuffer :: Buffer a
emptyBuffer = Buffer (ForeignPtr nullAddr# $ PlainForeignPtr noFinalizers) ReadBuffer 0 0

noFinalizers :: IORef Finalizers
noFinalizers = reallyUnsafeNewIORef NoFinalizers

-- | This should only be called on compile-time constants
reallyUnsafeNewIORef :: a -> IORef a
reallyUnsafeNewIORef = unsafeDupablePerformIO . newIORef
