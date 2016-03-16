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
  -- * General
  Decoder,
  equals,
  satisfies,
  matches,
)
where

import HTTPResponseDecoder.Prelude
import qualified Network.HTTP.Client
import qualified Network.HTTP.Types
import qualified Success.Pure
import qualified Data.HashMap.Strict
import qualified Data.CaseInsensitive
import qualified HTTPResponseDecoder.BodyReaders


run :: Response a -> Network.HTTP.Client.Response Network.HTTP.Client.BodyReader -> IO (Either Text a)
run (Response impl) =
  fmap (either (Left . fromMaybe defaultMessage) Right . Success.Pure.asEither) .
  impl
  where
    defaultMessage =
      ""


-- * Response
-------------------------

-- |
-- Response decoder.
newtype Response a =
  Response (Network.HTTP.Client.Response Network.HTTP.Client.BodyReader -> IO (Success Text a))
  deriving (Functor)

-- |
-- Composes a Response decoder from Head and Body decoders.
-- 
-- You can then merge the tuple in the result using the Functor interface.
headAndBody :: Head a -> Body b -> Response (a, b)
headAndBody (Head (Decoder (ReaderT responseToSuccess))) (Body bodyToIOSuccess) =
  Response $
  (liftA2 . liftA2 . liftA2) (,) (pure . responseToSuccess) (bodyToIOSuccess . responseToBody)
  where
    responseToBody =
      Network.HTTP.Client.responseBody


-- * Head
-------------------------

-- |
-- Response head decoder.
-- 
-- Supports the 'Applicative' and 'Alternative' interfaces.
newtype Head a =
  Head (forall body. Decoder (Network.HTTP.Client.Response body) a)
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

statusCode :: Decoder Int a -> Head a
statusCode decoder =
  Head $
  lmap mapping decoder
  where
    mapping =
      Network.HTTP.Types.statusCode .
      Network.HTTP.Client.responseStatus

httpVersion :: Decoder (Int, Int) a -> Head a
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
  Headers (Decoder (HashMap ByteString ByteString) a)
  deriving (Functor, Applicative, Alternative)

header :: ByteString -> Decoder ByteString a -> Headers a
header name headerDecoder =
  Headers $
  decoder
  where
    decoder =
      headerDecoder . lookup
      where
        lookup =
          matches $
          \hashMap ->
            Data.HashMap.Strict.lookup foldedName hashMap &
            maybe (Left ("Header " <> fromString (show foldedName) <> " not found")) Right
          where
            foldedName =
              Data.CaseInsensitive.foldCase name

contentType :: Decoder ByteString a -> Headers a
contentType =
  header "content-type"


-- * Body
-------------------------

-- |
-- Body decoder.
newtype Body a =
  Body (IO ByteString -> IO (Success Text a))
  deriving (Functor)

bodyStream :: (IO ByteString -> IO (Either Text a)) -> Body a
bodyStream reader =
  Body $
  fmap (either Success.Pure.failure Success.Pure.success) .
  reader

bodyBytes :: Decoder ByteString a -> Body a
bodyBytes (Decoder (ReaderT inputToSuccess)) =
  Body $
  fmap inputToSuccess .
  HTTPResponseDecoder.BodyReaders.bytes


-- * General Decoder
-------------------------

-- |
-- A general decoder, which abstracts over the input type.
newtype Decoder a b =
  Decoder (ReaderT a (Success Text) b)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

instance Category Decoder where
  {-# INLINE id #-}
  id =
    Decoder $
    ReaderT $
    Success.Pure.success
  (.) (Decoder (ReaderT successFn2)) (Decoder (ReaderT successFn1)) =
    Decoder $
    ReaderT $
    successFn1 >=> successFn2

instance Profunctor Decoder where
  {-# INLINE lmap #-}
  lmap fn (Decoder (ReaderT successFn)) =
    Decoder (ReaderT (successFn . fn))
  {-# INLINE rmap #-}
  rmap =
    fmap

equals :: Eq a => a -> Decoder a ()
equals reference =
  Decoder $
  ReaderT $
  \input ->
    if input == reference
      then Success.Pure.success ()
      else Success.Pure.failure "The input doesn't match the predicate"

satisfies :: (a -> Bool) -> Decoder a ()
satisfies predicate =
  Decoder $
  ReaderT $
  \input ->
    if predicate input
      then Success.Pure.success ()
      else Success.Pure.failure "The input doesn't satisfy the predicate"

matches :: (a -> Either Text b) -> Decoder a b
matches match =
  Decoder $
  ReaderT $
  \input ->
    match input &
    either Success.Pure.failure Success.Pure.success
