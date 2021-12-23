module Codec.MIME.QuotedPrintable
  ( QPPart (..),
    QP (..),
    toQP,
    qpBuilder,
  )
where

import Data.ByteString.Builder (Builder, char8)
import Data.ByteString.Lazy.Char8 qualified as B8L
import Data.Sequence (pattern Empty, pattern (:<|), pattern (:|>))

data QPPart
  = Plain Builder
  | Escape Builder
  | Newline
  | Tab
  | Space

newtype QP = QP (Seq QPPart)

instance Semigroup QP where
  QP Empty <> x = x
  x <> QP Empty = x
  QP (x :|> Plain b) <> QP (Plain b' :<| y) =
    QP (x :|> Plain (b <> b')) <> QP y
  QP (x :|> Escape b) <> QP (Escape b' :<| y) =
    QP (x :|> Escape (b <> b')) <> QP y
  QP x <> QP (qp :<| y) =
    QP (x :|> qp) <> QP y

instance Monoid QP where mempty = QP Empty

-- | Encode a lazy 'ByteString' as quoted-printable.
toQP :: Bool -> B8L.ByteString -> QP
toQP text = B8L.foldl' (\acc c -> acc <> qp c) mempty
  where
    qp :: Char -> QP
    qp = \case
      '\t' -> quip Tab
      '\n' -> quip $ if text then Newline else Escape (char8 '\n')
      '\r' -> if text then QP Empty else quip $ Escape (char8 '\r')
      ' ' -> quip Space
      c ->
        quip $
          if c `notElem` ['.', '='] && '!' <= c && c <= '~'
            then Plain (char8 c)
            else Escape (char8 c)
      where
        quip x = QP $ pure x

-- | Convert a quoted-printable encoding to a 'ByteString' 'Builder'.
qpBuilder :: QP -> Builder
qpBuilder (QP qps) = flip foldMap' qps $ \case
  Plain bs -> bs
  Escape bs -> bs
  Newline -> "\r\n"
  Tab -> "=09"
  Space -> "=20"
