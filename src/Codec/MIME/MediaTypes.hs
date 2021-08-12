module Codec.MIME.MediaTypes (
    MIMEType (..),
    mimetype,
    MediaType (..),
    mediatype,
    Multipart (..),
    multipart,
    pattern TextPlain,
    pattern MultipartRelated,
) where

-- | The value of the "Content-Type" header along with associated parameters.
data MIMEType = MIMEType
    { mediaType :: MediaType
    , mimeParams :: [(Text, Text)]
    }
    deriving (Eq)

-- | Get the proper 'Text' value for a 'MIMEType'.
mimetype :: MIMEType -> Text
mimetype MIMEType{..} =
    mediatype mediaType <> foldMap (\(name, val) -> fold ["; ", name, "=\"", val, "\""]) mimeParams

-- | The media type for the content beneath the header.
-- See [the IANA website](https://www.iana.org/assignments/media-types/media-types.xhtml)
-- to decide which content type is right for your data.
data MediaType
    = Application Text
    | Audio Text
    | Font Text
    | Image Text
    | Model Text
    | Message Text
    | Multipart Multipart
    | Text Text
    | Video Text
    | MediaOther Text Text
    deriving (Eq, Show)

-- | Get the proper 'Text' value for a 'MediaType'.
mediatype :: MediaType -> Text
mediatype = \case
    Application t -> "application/" <> t
    Audio t -> "audio/" <> t
    Font t -> "font/" <> t
    Image t -> "image/" <> t
    Model t -> "model/" <> t
    Message t -> "message/" <> t
    Multipart m -> "multipart/" <> multipart m
    Text t -> "text/" <> t
    Video t -> "video/" <> t
    MediaOther o t -> o <> "/" <> t

-- | Typical values of multipart media types.
-- See [the IANA website](https://www.iana.org/assignments/media-types/media-types.xhtml#multipart)
-- for a complete list.
data Multipart
    = Alternative
    | Byteranges
    | Digest
    | Encrypted
    | FormData
    | Mixed
    | Parallel
    | Related
    | Signed
    | Extension Text
    | MultipartOther Text
    deriving (Eq, Show)

-- | Get the proper 'Text' value for a 'Multipart' value.
multipart :: Multipart -> Text
multipart = \case
    Alternative -> "alternative"
    Byteranges -> "byteranges"
    Digest -> "digest"
    Encrypted -> "encrypted"
    FormData -> "form-data"
    Mixed -> "mixed"
    Parallel -> "parallel"
    Related -> "related"
    Signed -> "signed"
    Extension txt -> txt
    MultipartOther txt -> txt

pattern TextPlain :: MediaType
pattern TextPlain = Text "plain"

pattern MultipartRelated :: MediaType
pattern MultipartRelated = Multipart Related
