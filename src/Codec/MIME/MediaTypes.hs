module Codec.MIME.MediaTypes
  ( ContentType (..),
    contenttype,
    parseContentType,
    contentTypeP,
    MediaType (..),
    mediatype,
    parseMediaType,
    mediaTypeP,
    Multipart (..),
    multipart,
    parseMultipart,
    multipartP,
    pattern ApplicationPdf,
    pattern TextPlain,
    pattern TextHtml,
    pattern MultipartRelated,
  )
where

import Data.Text qualified as Text
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.Text (Parser)

-- | The value of the "Content-Type" header along with associated parameters.
data ContentType = ContentType
  { mediaType :: MediaType,
    contentParams :: [(Text, Text)]
  }
  deriving (Eq)

-- | Get the proper 'Text' value for a 'ContentType'.
contenttype :: ContentType -> Text
contenttype ContentType {..} =
  mediatype mediaType <> foldMap (\(name, val) -> fold ["; ", name, "=\"", val, "\""]) contentParams

parseContentType :: Text -> Either ParseError ContentType
parseContentType = parse contentTypeP ""

contentTypeP :: Parser ContentType
contentTypeP = do
  mediaType <- mediaTypeP <* optional (char ';')
  contentParams <- many $ on (,) toText <$> manyTill accepted (char '=') <*> many accepted
  pure ContentType {..}
  where
    accepted = try alphaNum <|> oneOf "-_."

-- |
-- The media type for the content beneath the header.
--
-- A media type may have multiple related subtypes separated by @+@ characters.
--
-- See [the IANA website](https://www.iana.org/assignments/media-types/media-types.xhtml)
-- to decide which content type is right for your data.
data MediaType
  = Application [Text]
  | Audio [Text]
  | Font [Text]
  | Image [Text]
  | Message [Text]
  | Model [Text]
  | Multipart Multipart
  | Text [Text]
  | Video [Text]
  deriving (Eq, Show)

-- | Get the proper 'Text' value for a 'MediaType'.
mediatype :: MediaType -> Text
mediatype = \case
  Application ts -> "application/" <> Text.intercalate "+" ts
  Audio ts -> "audio/" <> Text.intercalate "+" ts
  Font ts -> "font/" <> Text.intercalate "+" ts
  Image ts -> "image/" <> Text.intercalate "+" ts
  Message ts -> "message/" <> Text.intercalate "+" ts
  Model ts -> "model/" <> Text.intercalate "+" ts
  Multipart m -> "multipart/" <> multipart m
  Text ts -> "text/" <> Text.intercalate "+" ts
  Video ts -> "video/" <> Text.intercalate "+" ts

parseMediaType :: Text -> Either ParseError MediaType
parseMediaType = parse mediaTypeP ""

mediaTypeP :: Parser MediaType
mediaTypeP =
  choice . map try $
    [ applicationP,
      audioP,
      fontP,
      imageP,
      messageP,
      modelP,
      multipartP',
      textP,
      videoP
    ]
  where
    applicationP =
      Application . map toText <$ string "application/" <*> (many accepted `sepBy` try (char '+'))
    audioP =
      Audio . map toText <$ string "audio/" <*> (many accepted `sepBy` try (char '+'))
    fontP =
      Font . map toText <$ string "font/" <*> (many accepted `sepBy` try (char '+'))
    imageP =
      Image . map toText <$ string "image/" <*> (many accepted `sepBy` try (char '+'))
    messageP =
      Message . map toText <$ string "message/" <*> (many accepted `sepBy` try (char '+'))
    modelP =
      Model . map toText <$ string "model/" <*> (many accepted `sepBy` try (char '+'))
    multipartP' =
      Multipart <$> (string "multipart/" *> multipartP)
    textP =
      Text . map toText <$ string "text/" <*> (many accepted `sepBy` try (char '+'))
    videoP =
      Video . map toText <$ string "video/" <*> (many accepted `sepBy` try (char '+'))

    accepted = try alphaNum <|> oneOf "-_."

-- |
-- Typical values of multipart media types.
-- See [the IANA website](https://www.iana.org/assignments/media-types/media-types.xhtml#multipart)
-- for a complete list.
data Multipart
  = Alternative
  | Byteranges
  | Digest
  | Encrypted
  | Example
  | FormData
  | Mixed
  | Multilingual
  | Parallel
  | Related
  | Report
  | Signed
  | VoiceMessage
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Get the proper 'Text' value for a 'Multipart' value.
multipart :: Multipart -> Text
multipart = \case
  Alternative -> "alternative"
  Byteranges -> "byteranges"
  Digest -> "digest"
  Encrypted -> "encrypted"
  Example -> "example"
  FormData -> "form-data"
  Mixed -> "mixed"
  Multilingual -> "multilingual"
  Parallel -> "parallel"
  Related -> "related"
  Report -> "report"
  Signed -> "signed"
  VoiceMessage -> "voice-message"

parseMultipart :: Text -> Either ParseError Multipart
parseMultipart = parse multipartP ""

multipartP :: Parser Multipart
multipartP =
  choice . map try $
    [ Alternative <$ string "alternative",
      Byteranges <$ string "byteranges",
      Digest <$ string "digest",
      Encrypted <$ string "encrypted",
      Example <$ string "example",
      FormData <$ string "form-data",
      Mixed <$ string "mixed",
      Multilingual <$ string "multilingual",
      Parallel <$ string "parallel",
      Related <$ string "related",
      Report <$ string "report",
      Signed <$ string "signed",
      VoiceMessage <$ string "voice-message"
    ]

pattern ApplicationPdf :: MediaType
pattern ApplicationPdf = Application ["pdf"]

pattern TextPlain :: MediaType
pattern TextPlain = Text ["plain"]

pattern TextHtml :: MediaType
pattern TextHtml = Text ["html"]

pattern MultipartRelated :: MediaType
pattern MultipartRelated = Multipart Related
