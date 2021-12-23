module Network.SMTP.Auth
  ( Username (..),
    Password,
    AuthType (..),
    encodeLogin,
    auth,
  )
where

import Crypto.Hash.Algorithms (MD5)
import Crypto.MAC.HMAC (HMAC, hmac)
import Data.ByteArray (copyAndFreeze)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as B8
import Data.Char (isAscii)
import Data.Text qualified as T
import Text.Show (Show (..))

data AuthType
  = PLAIN
  | LOGIN
  | CRAM_MD5
  deriving (Eq)

instance Show AuthType where
  show = \case
    PLAIN -> "PLAIN"
    LOGIN -> "LOGIN"
    CRAM_MD5 -> "CRAM-MD5"

newtype Username = Username Text
  deriving (Eq)
  deriving newtype (Show)

newtype Password = Password Text
  deriving (Eq)

instance Show Password where
  show (Password u) = replicate (T.length u) '*'

ascii :: Text -> ByteString
ascii t = case T.partition isAscii t of
  (yes, no) -> if T.null no then encodeUtf8 yes else error $ "expected ASCII but got: " <> no

encodeLogin :: Username -> Password -> (ByteString, ByteString)
encodeLogin (Username u) (Password p) = (B64.encode $ ascii u, B64.encode $ ascii p)

auth :: AuthType -> Text -> Username -> Password -> ByteString
auth at c user@(Username u) pw@(Password p) = case at of
  PLAIN -> B64.encode . ascii $ T.intercalate "\0" [u, u, p]
  LOGIN -> let (u', p') = encodeLogin user pw in B8.unwords [u', p']
  CRAM_MD5 -> B64.encode $ B8.unwords [ascii u, B16.encode $ hmacMD5 (ascii c) (ascii p)]
  where
    hmacMD5 :: ByteString -> ByteString -> ByteString
    hmacMD5 chlg pwd = copyAndFreeze (hmac pwd chlg :: HMAC MD5) (const $ pure ())
