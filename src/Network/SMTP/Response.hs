module Network.SMTP.Response (Response (..), ReplyCode (..)) where

newtype ReplyCode = ReplyCode Int
    deriving newtype (Eq, Ord, Show, Read, Enum, Num, Real, Integral)

data Response
    = OK
    | SystemStatus
    | HelpMessage
    | ServiceReady
    | ServiceClosing
    | UserNotLocal
    | CannotVerify
    | StartMailInput
    | ServiceNotAvailable
    | MailboxUnavailable
    | ErrorInProcessing
    | InsufficientSystemStorage
    | SyntaxError
    | ParameterError
    | CommandNotImplemented
    | BadSequence
    | ParameterNotImplemented
    | MailboxUnavailableError
    | UserNotLocalError
    | ExceededStorage
    | MailboxNotAllowed
    | TransactionFailed
    deriving (Eq, Show)
