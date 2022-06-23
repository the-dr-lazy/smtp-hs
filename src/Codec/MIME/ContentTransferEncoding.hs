module Codec.MIME.ContentTransferEncoding (
  ContentTransferEncoding (..),
  contenttransferencoding,
) where

-- |
-- The value of the "Content-Transfer-Encoding" header,
-- can be "base64" or "quoted-printable".
-- If you need more encodings, please open a PR!
--
-- The 'Bool' associated to @QuotedPrintable@ specifies whether to encode
-- the content as text (@True@) or raw bytes (@False@).
data ContentTransferEncoding
  = Base64
  | QuotedPrintableText
  | QuotedPrintableBinary
  deriving (Eq, Show)

-- | Get the proper 'Text' value for a 'ContentTransferEncoding'.
contenttransferencoding :: ContentTransferEncoding -> Text
contenttransferencoding = \case
  Base64 -> "base64"
  _ -> "quoted-printable"
