module HTTPResponseDecoder.BodyReaders
where

import HTTPResponseDecoder.Prelude
import qualified ByteString.TreeBuilder
import qualified Data.ByteString


{-# INLINE bytes #-}
bytes :: IO ByteString -> IO ByteString
bytes =
  fmap ByteString.TreeBuilder.toByteString .
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
