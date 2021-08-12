module Codec.MIME.QuotedPrintable (
    QPPart (..),
    QP (..),
    toQP,
    qpBuilder,
) where

import Data.ByteString.Builder (Builder, byteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Sequence ((|>), pattern Empty, pattern (:<|), pattern (:|>))

data QPPart
    = Plain ByteString
    | Escape ByteString
    | Newline
    | Tab
    | Space

newtype QP = QP (Seq QPPart)
instance Semigroup QP where
    QP Empty <> x = x
    x <> QP Empty = x
    QP (x :|> Plain b) <> QP (Plain b' :<| y) =
        QP (x |> Plain (b <> b')) <> QP y
    QP (x :|> Escape b) <> QP (Escape b' :<| y) =
        QP (x |> Escape (b <> b')) <> QP y
    QP x <> QP (qp :<| y) =
        QP (x |> qp) <> QP y
instance Monoid QP where mempty = QP Empty

toQP :: Bool -> BSL.ByteString -> QP
toQP text = BSL.foldl' (\acc w8 -> acc <> qp w8) mempty
  where
    qp :: Word8 -> QP
    qp = \case
        9 -> quip Tab
        10 -> quip $ if text then Newline else Escape (one 10)
        13 -> if text then QP Empty else quip $ Escape (one 13)
        0x20 -> quip Space
        w ->
            quip $
                if w `notElem` [46, 61] && 33 <= w && w <= 126
                    then Plain (one w)
                    else Escape (one w)
      where
        quip x = QP $ pure x

qpBuilder :: QP -> Builder
qpBuilder (QP qps) = flip foldMap' qps $ \case
    Plain bs -> byteString bs
    Escape bs -> byteString bs
    Newline -> "\r\n"
    Tab -> "=09"
    Space -> "=20"
