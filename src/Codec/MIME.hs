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
    SomePart (..),
    somePart,
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

-- | The multiplicity of a 'Part'.
data Mult = One | Many

-- |
-- A 'Part' is an abstract representation of a part of an email; it could represent
-- an attachment, a message body, or even contain several, related sub-parts, such as
-- different translations of the same content.
--
-- Aside from its content, a 'Part' has a 'MIMEType', possibly a 'Disposition',
-- possibly a 'ContentTransferEncoding', possibly a "location" (or URI) where it
-- can be found, and perhaps additional headers.
data Part mult = Part
    { mimeType :: MIMEType
    , disp :: Maybe Disposition
    , encoding :: Maybe ContentTransferEncoding
    , location :: Maybe Text
    , partheaders :: [(Text, Text)]
    , partcontent :: PartContent mult
    }

-- |
-- The content of a 'Part' is either standalone, or it can be a single nested
-- collection of standalone parts.
data PartContent (mult :: Mult) where
    Single :: BSL.ByteString -> PartContent 'One
    Multiple :: NonEmpty (Part 'One) -> PartContent 'Many

-- |
-- An intermediate step between the datatype representation of a 'Part' and the
-- raw 'ByteString' representing its section of the email message.
data PartBuilder = PartBuilder
    { headers :: [(Text, Text)]
    , bsbuilder :: Builder
    }

-- | Connect multiple standalone 'Part's into a @multipart/related@ 'Part'.
related :: NonEmpty (Part 'One) -> Part 'Many
related parts = Part (MIMEType (Multipart Related) []) Nothing Nothing Nothing [] (Multiple parts)

encodeEscapedUtf8 :: Text -> Builder
encodeEscapedUtf8 t = fold ["=?utf-8?Q?", byteString $ rfc2822 t, "?="]

-- | Sanitize the header name and value before preparing for 'ByteString' conversion.
buildHeaders :: (Text, Text) -> Builder
buildHeaders (hname, hval) =
    fold
        [ byteString . encodeUtf8 $ T.filter (\w -> w /= ':' && '!' <= w && w <= '~') hname
        , ": "
        , if T.all isAscii hval then byteString $ encodeUtf8 hval else encodeEscapedUtf8 hval
        , "\n"
        ]

-- | Prepare a standalone 'Part' for 'ByteString' conversion.
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

-- | Prepare a nested 'Part' for 'ByteString' conversion. A 'Boundary' delineator is required.
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

-- | Delay the handling of the multiplicity of a part for the purposes of encoding the entire message.
data SomePart
    = SPOne (Part 'One)
    | SPMany (Part 'Many)

instance Eq SomePart where _ == _ = False
instance Ord SomePart where
    SPOne part `compare` SPOne part' = fmap dispType (disp part) `compare` fmap dispType (disp part')
    SPOne part `compare` SPMany part' = fmap dispType (disp part) `compare` fmap dispType (disp part')
    SPMany part `compare` SPOne part' = fmap dispType (disp part) `compare` fmap dispType (disp part')
    SPMany part `compare` SPMany part' = fmap dispType (disp part) `compare` fmap dispType (disp part')

-- | Wrap the multiplicity of a 'Part'.
somePart :: Part mult -> SomePart
somePart p@Part{partcontent = Single _} = SPOne p
somePart p@Part{partcontent = Multiple _} = SPMany p

-- | Build a message from a collection of parts of arbitrary multiplicity.
--
-- The 'Multipart' value is ignored in the case of a standalone 'Part', but describes
-- the MIME type in the nested case. A good default is 'Related'.
partBuilder :: (MonadRandom m) => Multipart -> NonEmpty SomePart -> m PartBuilder
partBuilder _ (a :| []) = case a of
    SPOne p -> pure $ singleBuilder p
    SPMany p -> (`multipleBuilder` p) <$> getRandom
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

-- | Create a 'PartBuilder' for the entire message given the 'PartBuilder's for each of its parts.
-- The result will have a MIME type of @multipart/mixed@.
mixedParts :: (MonadRandom m) => NonEmpty PartBuilder -> m PartBuilder
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

-- | 'ToSinglePart' captures the data-specific method to attach itself to an email message.
--
-- For example, @Html@ values are encoded as @quoted-printable@ text, whereas a 'Text' value
-- is simply converted to UTF-8. Files may have their own appropriate MIME types, so be sure
-- to declare this instance for its representation if you plan to send it via email.
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

-- | Convert a value to a 'Part' with the default encoding for its type.
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

-- | Convert a file to a 'Part', with the specified file path, file name and media type.
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

-- | Add the image at the specified file path to an email message.
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
