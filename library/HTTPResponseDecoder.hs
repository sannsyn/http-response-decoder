-- |
-- A DSL of HTTP-response decoders.
module HTTPResponseDecoder
(
  run,
  -- * Response
  Response,
  headAndBody,
  -- * Head
  Head,
  statusCode,
  httpVersion,
  headers,
  -- * Headers
  Headers,
  header,
  contentType,
  -- * Body
  Body,
  bodyStream,
  bodyBytes,
  bodyLazyBytes,
  -- * Matcher
  Matcher.Matcher,
  Matcher.equals,
  Matcher.satisfies,
  Matcher.converts,
)
where

import HTTPResponseDecoder.Prelude
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types
import qualified Data.ByteString.Lazy
import qualified Data.HashMap.Strict
import qualified Data.CaseInsensitive
import qualified HTTPResponseDecoder.BodyReaders
import qualified Matcher


run :: Response a -> Network.HTTP.Client.Response Network.HTTP.Client.BodyReader -> IO (Either Text a)
run (Response impl) =
  impl


-- * Response
-------------------------

-- |
-- Response decoder.
newtype Response a =
  Response (Network.HTTP.Client.Response Network.HTTP.Client.BodyReader -> IO (Either Text a))
  deriving (Functor)

-- |
-- Composes a Response decoder from Head and Body decoders.
-- 
-- You can then merge the tuple in the result using the Functor interface.
headAndBody :: Head a -> Body b -> Response (a, b)
headAndBody (Head headMatcher) (Body bodyToIOEither) =
  Response $
  (liftA2 . liftA2 . liftA2) (,) (pure . responseToEither) (bodyToIOEither . responseToBody)
  where
    responseToBody =
      Network.HTTP.Client.responseBody
    responseToEither =
      Matcher.run headMatcher


-- * Head
-------------------------

-- |
-- Response head decoder.
-- 
-- Supports the 'Applicative' and 'Alternative' interfaces.
newtype Head a =
  Head (forall body. Matcher.Matcher (Network.HTTP.Client.Response body) a)
  deriving (Functor)

instance Applicative Head where
  {-# INLINE pure #-}
  pure a =
    Head (pure a)
  {-# INLINE (<*>) #-}
  (<*>) (Head decoder1) (Head decoder2) =
    Head (decoder1 <*> decoder2)

instance Alternative Head where
  {-# INLINE empty #-}
  empty =
    Head empty
  {-# INLINE (<|>) #-}
  (<|>) (Head decoder1) (Head decoder2) =
    Head (decoder1 <|> decoder2)

statusCode :: Matcher.Matcher Int a -> Head a
statusCode decoder =
  Head $
  lmap mapping decoder
  where
    mapping =
      Network.HTTP.Types.statusCode .
      Network.HTTP.Client.responseStatus

httpVersion :: Matcher.Matcher (Int, Int) a -> Head a
httpVersion decoder =
  Head $
  lmap mapping decoder
  where
    mapping =
      httpVersionToTuple .
      Network.HTTP.Client.responseVersion
      where
        httpVersionToTuple (Network.HTTP.Types.HttpVersion major minor) =
          (major, minor)

headers :: Headers a -> Head a
headers (Headers decoder) =
  Head $
  lmap mapping decoder
  where
    mapping =
      Data.HashMap.Strict.fromList .
      map foldKeyCase .
      Network.HTTP.Client.responseHeaders
      where
        foldKeyCase (k, v) =
          (Data.CaseInsensitive.foldedCase k, v)


-- * Headers
-------------------------

-- |
-- Response headers decoder.
newtype Headers a =
  Headers (Matcher.Matcher (HashMap ByteString ByteString) a)
  deriving (Functor, Applicative, Alternative)

header :: ByteString -> Matcher.Matcher ByteString a -> Headers a
header name headerMatcher =
  Headers $
  decoder
  where
    decoder =
      headerMatcher . lookup
      where
        lookup =
          Matcher.converts $
          \hashMap ->
            Data.HashMap.Strict.lookup foldedName hashMap &
            maybe (Left ("Header " <> fromString (show foldedName) <> " not found")) Right
          where
            foldedName =
              Data.CaseInsensitive.foldCase name

contentType :: Matcher.Matcher ByteString a -> Headers a
contentType =
  header "content-type"


-- * Body
-------------------------

-- |
-- Body decoder.
newtype Body a =
  Body (IO ByteString -> IO (Either Text a))
  deriving (Functor)

bodyStream :: (IO ByteString -> IO (Either Text a)) -> Body a
bodyStream reader =
  Body $
  reader

bodyBytes :: Matcher.Matcher ByteString a -> Body a
bodyBytes matcher =
  Body $
  fmap (Matcher.run matcher) .
  HTTPResponseDecoder.BodyReaders.bytes

bodyLazyBytes :: Matcher.Matcher Data.ByteString.Lazy.ByteString a -> Body a
bodyLazyBytes matcher =
  Body $
  fmap (Matcher.run matcher) .
  HTTPResponseDecoder.BodyReaders.lazyBytes

