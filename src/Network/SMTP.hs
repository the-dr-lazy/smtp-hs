{-# LANGUAGE FlexibleContexts #-}

module Network.SMTP where

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B8
import Data.Char (isDigit)
import Network.BSD (getHostName)
import Network.Connection (Connection, TLSSettings (..))
import Network.Connection qualified as Cxn
import Network.SMTP.Auth as SMTP
import Network.SMTP.Command as SMTP
import Network.SMTP.Response as SMTP
import Network.Socket
import Relude.Extra
import Relude.Unsafe (read)

defaulttls :: TLSSettings
defaulttls = Cxn.TLSSettingsSimple False False False

response :: (MonadReader Connection m, MonadIO m) => m (ReplyCode, ByteString)
response = do
    l <- liftIO . Cxn.connectionGetLine 1000 =<< ask
    let (digits, body) = B8.span isDigit l
    case B8.uncons body of
        Just ('-', bs) -> second ((bs <> "\n") <>) <$> response
        Just (_, bs) -> pure (read (B8.unpack digits), bs)
        Nothing -> pure (read (B8.unpack digits), mempty)

replycode :: (MonadReader Connection m, MonadIO m) => m ReplyCode
replycode = fst <$> response

cputLine :: (MonadReader Connection m, MonadIO m) => ByteString -> m ()
cputLine bs = liftIO . (`Cxn.connectionPut` (bs <> "\r\n")) =<< ask

sendCommand :: (MonadReader Connection m, MonadIO m) => Command -> m (ReplyCode, ByteString)
sendCommand cmd = case cmd of
    DATA bs -> do
        cputLine "DATA"
        code <- replycode
        unless (code == 354) . liftIO $ fail "This server is not configured to receive data."
        traverse_ (cputLine . padDot . stripCR) $ BS.split lf bs
        cputLine "."
        response
      where
        padDot, stripCR :: ByteString -> ByteString
        stripCR s = BS.stripSuffix "\r" s ?: s
        padDot s = "." <> (BS.stripPrefix "." s ?: s)
        lf :: Word8
        lf = fromIntegral $ ord '\n'
    AUTH LOGIN user pw -> do
        let (u, p) = encodeLogin user pw
        cputLine "AUTH LOGIN"
        void response
        cputLine u
        void response
        cputLine p
        rsp@(code, _) <- response
        rsp <$ unless (code == 235) (liftIO $ fail "Authentication failed.")
    AUTH authtype user pw -> do
        cputLine $ "AUTH " <> show authtype
        (code, msg) <- response
        unless (code == 334) . liftIO $ fail "Authentication failed."
        cputLine $ auth authtype (decodeUtf8 msg) user pw
        response
    _ -> cputLine c >> response
      where
        c = case cmd of
            HELO bs -> "HELO " <> bs
            EHLO bs -> "EHLO " <> bs
            MAIL bs -> "MAIL FROM:<" <> bs <> ">"
            RCPT bs -> "RCPT TO:<" <> bs <> ">"
            EXPN bs -> "EXPN " <> bs
            VRFY bs -> "VRFY " <> bs
            HELP "" -> "HELP\r\n"
            HELP bs -> "HELP " <> bs
            x -> show x

closeSMTP :: (MonadReader Connection m, MonadIO m) => m ()
closeSMTP = do
    void $ sendCommand QUIT
    liftIO . Cxn.connectionClose =<< ask

command :: (MonadReader Connection m, MonadIO m) => Int -> Command -> ReplyCode -> m (Maybe ByteString)
command 0 _ _ = pure Nothing
command times cmd expect = do
    (code, msg) <- sendCommand cmd
    if code == expect
        then pure $ Just msg
        else command (prev times) cmd expect

commandOrQuit :: (MonadReader Connection m, MonadIO m) => Int -> Command -> ReplyCode -> m ByteString
commandOrQuit 1 cmd expect = do
    (code, msg) <- sendCommand cmd
    if code == expect
        then pure msg
        else
            (closeSMTP >>) . liftIO . fail $
                "Unexpected reply to \""
                    <> show cmd
                    <> "\": Expected "
                    <> show expect
                    <> " but got \""
                    <> show code
                    <> ": "
                    <> decodeUtf8 msg
                    <> "\""
commandOrQuit times cmd expect = do
    (code, msg) <- sendCommand cmd
    if code == expect
        then pure msg
        else commandOrQuit (prev times) cmd expect

smtpconnect :: (MonadReader Connection m, MonadIO m) => IO HostName -> m [ByteString]
smtpconnect gethostname = do
    c <- replycode
    unless (c == 220) $ do
        cxn <- ask
        liftIO $ Cxn.connectionClose cxn >> fail "Could not connect to server"
    sender <- liftIO gethostname
    mehlo <- command 3 (EHLO $ encodeUtf8 sender) 250
    case mehlo of
        Just ehlo -> pure . drop 1 $ B8.lines ehlo
        Nothing -> do
            mhelo <- command 3 (HELO $ encodeUtf8 sender) 250
            pure $ maybe [] (drop 1 . B8.lines) mhelo

smtpconnectSTARTTLS :: (MonadReader Connection m, MonadIO m) => IO HostName -> Cxn.ConnectionContext -> TLSSettings -> m [ByteString]
smtpconnectSTARTTLS gethostname ctx tls = do
    c <- replycode
    unless (c == 220) $ do
        cxn <- ask
        liftIO $ Cxn.connectionClose cxn >> fail "Could not connect to server"
    sender <- liftIO gethostname
    void $ commandOrQuit 3 (EHLO $ encodeUtf8 sender) 250
    void $ commandOrQuit 1 STARTTLS 220
    void $ liftIO . flip (Cxn.connectionSetSecure ctx) tls =<< ask
    drop 1 . B8.lines <$> commandOrQuit 1 (EHLO $ encodeUtf8 sender) 250

connectSMTP' ::
    (MonadIO m) =>
    HostName ->
    Maybe PortNumber ->
    Maybe (IO HostName) ->
    Maybe TLSSettings ->
    m (Connection, [ByteString])
connectSMTP' hostname ((?: 25) -> port) ((?: getHostName) -> gethostname) mtls = do
    ctx <- liftIO Cxn.initConnectionContext
    cxn <- liftIO $ Cxn.connectTo ctx (Cxn.ConnectionParams hostname port mtls Nothing)
    (cxn,) <<$>> usingReaderT cxn $ smtpconnect gethostname

connectSMTPSTARTTLS' ::
    (MonadIO m) =>
    HostName ->
    Maybe PortNumber ->
    Maybe (IO HostName) ->
    Maybe TLSSettings ->
    m (Connection, [ByteString])
connectSMTPSTARTTLS' hostname ((?: 25) -> port) ((?: getHostName) -> gethostname) mtls = do
    ctx <- liftIO Cxn.initConnectionContext
    cxn <- liftIO $ Cxn.connectTo ctx (Cxn.ConnectionParams hostname port Nothing Nothing)
    (cxn,) <<$>> usingReaderT cxn $ smtpconnectSTARTTLS gethostname ctx (mtls ?: defaulttls)

connectSMTP :: (MonadIO m) => HostName -> m (Connection, [ByteString])
connectSMTP hostname = connectSMTP' hostname Nothing Nothing Nothing

connectSMTPS :: (MonadIO m) => HostName -> m (Connection, [ByteString])
connectSMTPS hostname = connectSMTP' hostname (Just 465) Nothing (Just defaulttls)

connectSMTPSTARTTLS :: (MonadIO m) => HostName -> m (Connection, [ByteString])
connectSMTPSTARTTLS hostname = connectSMTPSTARTTLS' hostname (Just 587) Nothing Nothing