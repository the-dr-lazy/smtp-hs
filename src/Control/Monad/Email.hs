{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Email (
    ConnectionMethod (..),
    SMTPSettings (..),
    MonadSMTP (..),
    HostName,
    PortNumber,
    module Network.SMTP.Email,
) where

import Control.Monad.Random
import Network.Connection (TLSSettings)
import Network.SMTP
import Network.SMTP.Auth
import Network.SMTP.Command
import Network.SMTP.Email
import Network.Socket (HostName, PortNumber)

data ConnectionMethod = SMTP | SMTPS | SMTPSTARTTLS
data SMTPSettings = SMTPSettings
    { hostname :: HostName
    , port :: Maybe PortNumber
    , cxmethod :: ConnectionMethod
    , tlsSettings :: Maybe TLSSettings
    , username :: Username
    , password :: Password
    }

class (MonadRandom m, MonadIO m) => MonadSMTP m where
    smtpSettings :: m SMTPSettings

    -- | Login to the SMTP server using the 'SMTPSettings', then render and send the email.
    sendMail :: Mail -> m ()
    sendMail m@Mail{..} = do
        SMTPSettings{..} <- smtpSettings
        let connect = case cxmethod of
                SMTP -> connectSMTP'
                SMTPS -> connectSMTP'
                SMTPSTARTTLS -> connectSMTPSTARTTLS'
        (cxn, _response) <- connect hostname port Nothing tlsSettings
        usingReaderT cxn $ do
            void $ commandOrQuit 1 (AUTH LOGIN username password) 235
            let from = emailByteString $ mailboxEmail mailFrom
                tos = map (emailByteString . mailboxEmail) $ mailTo <> mailCc <> mailBcc
            mrendered <- renderMail m
            case mrendered of
                Left er -> liftIO . fail $ show er
                Right mail -> do
                    void $ commandOrQuit 1 (MAIL from) 250
                    for_ tos $ \r -> commandOrQuit 1 (RCPT r) 250
                    void $ commandOrQuit 1 (DATA $ toStrict mail) 250
            closeSMTP
