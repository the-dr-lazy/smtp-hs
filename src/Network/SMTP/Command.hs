module Network.SMTP.Command (Command (..)) where

import Data.ByteString (ByteString)
import Network.SMTP.Auth (AuthType, Password, Username)

data Command
  = HELO ByteString
  | EHLO ByteString
  | MAIL ByteString
  | RCPT ByteString
  | DATA ByteString
  | EXPN ByteString
  | VRFY ByteString
  | HELP ByteString
  | AUTH AuthType Username Password
  | NOOP
  | RSET
  | QUIT
  | STARTTLS
  deriving (Eq, Show)
