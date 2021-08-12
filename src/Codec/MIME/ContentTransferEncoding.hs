module Codec.MIME.ContentTransferEncoding (
    ContentTransferEncoding (..),
    contenttransferencoding,
) where

data ContentTransferEncoding
    = Base64
    | QuotedPrintable Bool -- true/text, false/binary
    deriving (Eq, Show)

contenttransferencoding :: ContentTransferEncoding -> Text
contenttransferencoding = \case
    Base64 -> "base64"
    _ -> "quoted-printable"
