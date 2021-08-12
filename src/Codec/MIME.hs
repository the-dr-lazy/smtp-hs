{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Codec.MIME (
    module MIME,
    Part (..),
    related,
    PartContent (..),
    PartBuilder (..),
    ArbitraryPart (..),
    arbPart,
    encodeEscapedUtf8,
    buildHeaders,
    partBuilder,
    mixedParts,
    ToSinglePart (..),
    toSinglePart,
    filePart,
    imagePart,
) where

import Codec.MIME.Boundary as MIME
import Codec.MIME.ContentTransferEncoding as MIME
import Codec.MIME.Disposition as MIME
import Codec.MIME.MediaTypes as MIME
import Codec.MIME.QuotedPrintable as MIME
import Codec.MIME.TextEncoding as MIME
import Control.Monad (liftM3)
import Control.Monad.Random (MonadRandom (getRandom))
import Data.ByteString.Base64.Lazy qualified as B64L
import Data.ByteString.Builder (Builder, byteString, lazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Char (isAscii)
import Data.Text qualified as T
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Yesod.Content.PDF (PDF (pdfBytes))

data Mult = One | Many

data Part mult = Part
    { mimeType :: MIMEType
    , disp :: Maybe Disposition
    , encoding :: Maybe ContentTransferEncoding
    , location :: Maybe Text
    , partheaders :: [(Text, Text)]
    , partcontent :: PartContent mult
    }

data PartContent (mult :: Mult) where
    Single :: BSL.ByteString -> PartContent 'One
    Multiple :: NonEmpty (Part 'One) -> PartContent 'Many

data PartBuilder = PartBuilder
    { headers :: [(Text, Text)]
    , bsbuilder :: Builder
    }

related :: NonEmpty (Part 'One) -> Part 'Many
related parts = Part (MIMEType (Multipart Related) []) Nothing Nothing Nothing [] (Multiple parts)

encodeEscapedUtf8 :: Text -> Builder
encodeEscapedUtf8 t = fold ["=?utf-8?Q?", byteString $ rfc2822 t, "?="]

buildHeaders :: (Text, Text) -> Builder
buildHeaders (hname, hval) =
    fold
        [ byteString . encodeUtf8 $ T.filter (\w -> w /= ':' && w >= '!' && w <= '~') hname
        , ": "
        , if T.all isAscii hval then byteString $ encodeUtf8 hval else encodeEscapedUtf8 hval
        , "\n"
        ]

singleBuilder :: Part 'One -> PartBuilder
singleBuilder Part{partcontent = Single bs, ..} = PartBuilder{..}
  where
    headers =
        (("Content-Type", mimetype mimeType) :)
            . maybe id (\e -> (("Content-Transfer-Encoding", contenttransferencoding e) :)) encoding
            . maybe id (\d -> (("Content-Disposition", disposition d) :)) disp
            . maybe id (\loc -> (("Content-Location", loc) :)) location
            $ partheaders
    bsbuilder = case encoding of
        Just Base64 ->
            foldMap' ((<> "\r\n") . lazyByteString . B64L.encode) $
                unfoldr (liftM3 bool (const Nothing) (Just . BSL.splitAt 57) BSL.null) bs
        Just (QuotedPrintable txt) -> qpBuilder $ toQP txt bs
        Nothing -> lazyByteString bs

multipleBuilder :: Boundary -> Part 'Many -> PartBuilder
multipleBuilder (Boundary bdy) Part{partcontent = Multiple ps} = PartBuilder{..}
  where
    headers = [("Content-Type", mimetype $ MIMEType (Multipart Related) [("boundary", bdy)])]
    bsbuilder = foldMap (boundarypart bdy) ps <> "--" <> byteString (encodeUtf8 bdy) <> "--"

    boundarypart :: Text -> Part 'One -> Builder
    boundarypart bd Part{partcontent = Single bs, partheaders} =
        foldMap byteString ["--", encodeUtf8 bd, "\n"]
            <> foldMap buildHeaders partheaders
            <> "\n"
            <> lazyByteString bs
            <> "\n"

data ArbitraryPart
    = ArbOne (Part 'One)
    | ArbMany (Part 'Many)
instance Eq ArbitraryPart where _ == _ = False
instance Ord ArbitraryPart where
    ArbOne part `compare` ArbOne part' = fmap dispType (disp part) `compare` fmap dispType (disp part')
    ArbOne part `compare` ArbMany part' = fmap dispType (disp part) `compare` fmap dispType (disp part')
    ArbMany part `compare` ArbOne part' = fmap dispType (disp part) `compare` fmap dispType (disp part')
    ArbMany part `compare` ArbMany part' = fmap dispType (disp part) `compare` fmap dispType (disp part')

arbPart :: Part mult -> ArbitraryPart
arbPart p@Part{partcontent = Single _} = ArbOne p
arbPart p@Part{partcontent = Multiple _} = ArbMany p

partBuilder :: (MonadRandom m) => Multipart -> NonEmpty ArbitraryPart -> m PartBuilder
partBuilder _ (a :| []) = case a of
    ArbOne p -> pure $ singleBuilder p
    ArbMany p -> (`multipleBuilder` p) <$> getRandom
partBuilder m arbs = do
    pbs <- forM arbs $ partBuilder m . pure
    Boundary bdy <- getRandom
    let headers = [("Content-Type", mimetype $ MIMEType (Multipart m) [("boundary", bdy)])]
        bsbuilder = foldMap (boundarypart bdy) pbs <> "--" <> byteString (encodeUtf8 bdy) <> "--"
    pure PartBuilder{..}
  where
    boundarypart :: Text -> PartBuilder -> Builder
    boundarypart bdy PartBuilder{..} =
        foldMap byteString ["--", encodeUtf8 bdy, "\n"]
            <> foldMap buildHeaders headers
            <> "\n"
            <> bsbuilder
            <> "\n"

mixedParts :: (MonadRandom m) => [PartBuilder] -> m PartBuilder
mixedParts ps = do
    Boundary bdy <- getRandom
    let headers = [("Content-Type", mimetype $ MIMEType (Multipart Mixed) [("boundary", bdy)])]
        bsbuilder = foldMap (boundarypart bdy) ps <> "--" <> byteString (encodeUtf8 bdy) <> "--"
    pure PartBuilder{..}
  where
    boundarypart :: Text -> PartBuilder -> Builder
    boundarypart bdy PartBuilder{..} =
        foldMap byteString ["--", encodeUtf8 bdy, "\n"]
            <> foldMap buildHeaders headers
            <> "\n"
            <> bsbuilder
            <> "\n"

class ToSinglePart a where
    partMIMEType :: proxy a -> MIMEType
    default partMIMEType :: (ToText a) => proxy a -> MIMEType
    partMIMEType _ = MIMEType (Text "plain") [("charset", "utf-8")]

    partDisposition :: proxy a -> Maybe Disposition
    default partDisposition :: (ToText a) => proxy a -> Maybe Disposition
    partDisposition _ = Nothing

    partEncoding :: proxy a -> Maybe ContentTransferEncoding
    default partEncoding :: (ToText a) => proxy a -> Maybe ContentTransferEncoding
    partEncoding _ = Just $ QuotedPrintable True

    partContent :: a -> BSL.ByteString

toSinglePart :: forall a. (ToSinglePart a) => a -> Part 'One
toSinglePart a = Part{..}
  where
    mimeType = partMIMEType prox
    disp = partDisposition prox
    encoding = partEncoding prox
    location = Nothing
    partheaders = []
    partcontent = Single $ partContent a
    prox = Proxy @a

filePart :: (MonadIO m) => FilePath -> Text -> MediaType -> m (Part 'One)
filePart fp name media = do
    content <- readFileLBS fp
    let mimeType = MIMEType media [("charset", "utf-8")]
        disp = Just $ Disposition Attachment [FilenameStar name]
        encoding = Just Base64
        location = Just name
        partheaders = []
        partcontent = Single content
    pure Part{..}

imagePart :: (MonadIO m) => FilePath -> m (Part 'One)
imagePart fp = do
    content <- readFileLBS fp
    let name = T.takeWhileEnd (/= '/') $ toText fp
        ext = T.takeWhileEnd (/= '.') $ toText fp
        mimeType = MIMEType (Image ext) []
        disp = Just $ Disposition Inline [FilenameStar name]
        encoding = Just Base64
        location = Just name
        partheaders = []
        partcontent = Single content
    pure Part{..}

instance ToSinglePart Text where partContent = encodeUtf8
instance ToSinglePart Html where
    partMIMEType _ = MIMEType (Text "html") [("charset", "utf-8")]
    partDisposition _ = Nothing
    partEncoding _ = Just $ QuotedPrintable True
    partContent = renderHtml
instance ToSinglePart PDF where
    partMIMEType _ = MIMEType (Application "pdf") [("charset", "utf-8")]
    partDisposition _ = Just $ Disposition Attachment []
    partEncoding _ = Just Base64
    partContent = fromStrict . pdfBytes
