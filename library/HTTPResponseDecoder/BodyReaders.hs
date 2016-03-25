module HTTPResponseDecoder.BodyReaders
where

import HTTPResponseDecoder.Prelude
import qualified ByteString.TreeBuilder
import qualified Data.ByteString
import qualified Data.ByteString.Lazy


{-# INLINE bytes #-}
bytes :: IO ByteString -> IO ByteString
bytes =
  fmap ByteString.TreeBuilder.toByteString .
  builder

{-# INLINE lazyBytes #-}
lazyBytes :: IO ByteString -> IO Data.ByteString.Lazy.ByteString
lazyBytes =
  fmap ByteString.TreeBuilder.toLazyByteString .
  builder

{-# INLINABLE builder #-}
builder :: IO ByteString -> IO ByteString.TreeBuilder.Builder
builder chunk =
  loop mempty
  where
    loop builder =
      do
        chunk <- chunk
        if Data.ByteString.null chunk
          then return builder
          else loop (builder <> ByteString.TreeBuilder.byteString chunk)
