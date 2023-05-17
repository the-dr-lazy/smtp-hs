module Network.SMTP.Response (ReplyCode (..)) where

newtype ReplyCode = ReplyCode Int
  deriving newtype (Eq, Ord, Show, Read, Enum, Num, Real, Integral)
