module Codec.MIME.Disposition (
    Disposition (..),
    disposition,
    DispType (..),
    disptype,
    DispParam (..),
    dispparam,
) where

import Codec.MIME.TextEncoding (rfc5987)
import Data.Text qualified as T
import Data.Time.Compat (LocalTime (..), TimeOfDay (..), pattern YearMonthDay)

data Disposition = Disposition
    { dispType :: DispType
    , dispParams :: [DispParam]
    }
    deriving (Eq)

disposition :: Disposition -> Text
disposition Disposition{..} = disptype dispType <> foldMap (("; " <>) . dispparam) dispParams

data DispType
    = Inline
    | Attachment
    | DispOther Text
    deriving (Eq, Ord, Show)

disptype :: DispType -> Text
disptype = \case
    Inline -> "inline"
    Attachment -> "attachment"
    DispOther t -> t

data DispParam
    = Name Text
    | FilenameStar Text
    | Filename Text
    | Created LocalTime
    | Modified LocalTime
    | Read LocalTime
    | Size Text
    | Other Text Text
    deriving (Eq, Show)

dispparam :: DispParam -> Text
dispparam = \case
    Name t -> "name=\"" <> t <> "\""
    FilenameStar t -> "filename*=utf-8''" <> rfc5987 t
    Filename t -> "filename=\"" <> t <> "\""
    Created t -> "creation-date=\"" <> dmy24 t <> "\""
    Modified t -> "modification-date=\"" <> dmy24 t <> "\""
    Read t -> "read-date=\"" <> dmy24 t <> "\""
    Size t -> "size=" <> show t
    Other t t' -> t <> "=" <> t'

dmy24 :: LocalTime -> Text
dmy24 (LocalTime (YearMonthDay y m d) (TimeOfDay h mm s)) =
    T.unwords [tshow 0 d, month m, T.drop 2 $ tshow 4 y, T.intercalate ":" $ map (tshow 2) [h, mm, round s]]
  where
    tshow :: (Show a) => Int -> a -> Text
    tshow n = (\str -> T.replicate (n - T.length str) "0" <> str) . show
    month :: Int -> Text
    month = \case
        1 -> "Jan"
        2 -> "Feb"
        3 -> "Mar"
        4 -> "Apr"
        5 -> "May"
        6 -> "Jun"
        7 -> "Jul"
        8 -> "Aug"
        9 -> "Sep"
        10 -> "Oct"
        11 -> "Nov"
        _ -> "Dec"
