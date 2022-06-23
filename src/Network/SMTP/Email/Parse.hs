{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Network.SMTP.Email.Parse (
  Email,
  Mailbox (..),
  email,
  mailbox,
  mailboxes,
  unsafeEmail,
  domainPart,
  emailByteString,
  localPart,
  validEmail,
  validMailbox,
  validateEmail,
  validateMailbox,
  validateMailboxes,
) where

import Control.Arrow ((+++))
import Data.Text qualified as T
import Language.Haskell.TH (ExpQ, listE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Text.Parsec (
  ParsecT,
  Stream,
  char,
  crlf,
  endBy1,
  eof,
  many1,
  oneOf,
  parse,
  sepEndBy1,
  string,
  tab,
  (<?>),
 )
import Text.Parsec qualified as TP
import Text.Parsec.Text (Parser)
import Prelude hiding (group, (<|>))

data Mailbox = Mailbox
  { mailboxName :: Maybe Text
  , mailboxEmail :: Email
  }

-- |
-- Abstract data type representing an email address.
--
-- Use 'localPart' and 'domainPart' to extract those substrings,
-- or 'emailByteString' for the complete address in UTF-8.
data Email = Email !Text !Text

unsafeEmail :: Text -> Text -> Email
unsafeEmail = Email

email :: QuasiQuoter
email =
  QuasiQuoter
    { quoteExp = qemail emailexp
    , quotePat = error "email is not supported as a pattern"
    , quoteDec = error "email is not supported at top-level"
    , quoteType = error "email is not supported as a type"
    }
 where
  qemail p s =
    case validateEmail $ toText s of
      Left err -> error $ "Invalid quasi-quoted email address: " <> toText err
      Right e -> p e

emailexp :: Email -> ExpQ
emailexp e =
  let lp = localPart e
      dp = domainPart e
   in [|Email lp dp|]

mailbox :: QuasiQuoter
mailbox =
  QuasiQuoter
    { quoteExp = qmailbox mailboxexp
    , quotePat = error "mailbox is not supported as a pattern"
    , quoteDec = error "mailbox is not supported at top-level"
    , quoteType = error "mailbox is not supported as a type"
    }
 where
  qmailbox p s =
    case validateMailbox $ toText s of
      Left err -> error $ "Invalid quasi-quoted mailbox: " <> toText err
      Right e -> p e

mailboxexp :: Mailbox -> ExpQ
mailboxexp Mailbox{..} = [|Mailbox mailboxName $(emailexp mailboxEmail)|]

mailboxes :: QuasiQuoter
mailboxes =
  QuasiQuoter
    { quoteExp = qmailbox (listE . map mailboxexp . toList)
    , quotePat = error "mailboxes is not supported as a pattern"
    , quoteDec = error "mailboxes is not supported at top-level"
    , quoteType = error "mailboxes is not supported as a type"
    }
 where
  qmailbox p s =
    case validateMailboxes $ toText s of
      Left err -> error $ "Invalid quasi-quoted mailbox list: " <> toText err
      Right e -> p e

emailByteString :: Email -> ByteString
emailByteString (Email l d) = encodeUtf8 l <> "@" <> encodeUtf8 d

localPart, domainPart :: Email -> Text
localPart (Email l _) = l
domainPart (Email _ d) = d

validEmail :: Text -> Bool
validEmail = either (const False) (const True) . validateEmail

validMailbox :: Text -> Bool
validMailbox = either (const False) (const True) . validateMailbox

validateEmail :: Text -> Either String Email
validateEmail = (show +++ id) . parse (addrSpec <* eof) ""

validateMailbox :: Text -> Either String Mailbox
validateMailbox = (show +++ id) . parse (mailbox' <* eof) ""

validateMailboxes :: Text -> Either String (NonEmpty Mailbox)
validateMailboxes = (show +++ id) . parse (mailboxList <* eof) ""

-- Parsing

infix 2 ?>

(?>) :: (Alternative f) => Bool -> a -> f a
b ?> a = if b then pure a else empty

-- backtracking
infixl 4 <|>

(<|>) :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
a <|> b = TP.choice [TP.try a, TP.try b]

-- backtracking
choice :: (Stream s m t) => [ParsecT s u m a] -> ParsecT s u m a
choice ps = TP.choice $ map TP.try ps

tmany :: ParsecT s u m Char -> ParsecT s u m Text
tmany p = toText <$> TP.many p

tmany1 :: (Stream s m t) => ParsecT s u m Char -> ParsecT s u m Text
tmany1 p = toText <$> TP.many1 p

ranges :: [[Word8]] -> Parser Char
ranges rs = oneOf $ foldMap (map $ chr . fromIntegral) rs

vchar :: Parser Char
vchar = oneOf ['!' .. '~']

nul, sp, cr, lf :: Parser Char
nul = char '\0'
sp = char ' '
cr = char '\r'
lf = char '\n'

-- | Equivalent to @[0-9A-Za-z]@.
alphaNum :: Parser Char
alphaNum = ranges [[48 .. 57], [65 .. 90], [97 .. 122]]

wsp :: Parser Char
wsp = sp <|> tab

wsp1 :: Parser ()
wsp1 = void $ many1 wsp

-- 3.2.1

quotedpair :: Parser Text
quotedpair = (T.cons <$> char '\\' <*> fmap one (vchar <|> wsp)) <|> obsQP

-- 3.2.2

fws :: Parser ()
fws = optional (many wsp *> crlf) *> void (many1 wsp) <|> obsFws

ctext :: Parser Char
ctext = ranges [[33 .. 39], [42 .. 91], [93 .. 126]] <|> obsCtext

ccontent :: Parser Text
ccontent = fmap one ctext <|> quotedpair <|> comment

comment :: Parser Text
comment =
  fmap unwords $
    char '('
      *> many (optional fws *> ccontent)
      <* fws
      <* char ')'

cfws :: Parser ()
cfws = void (many1 (optional fws *> comment) <* fws) <|> fws

-- 3.2.3

atext :: Parser Char
atext = alphaNum <|> oneOf "!#$%&'*+/=?^_`{|}~-"

atom :: Parser Text
atom = optional cfws *> tmany1 atext <* optional cfws

dotAtomText :: Parser Text
dotAtomText = T.intercalate "." <$> tmany1 atext `endBy1` string "."

dotAtom :: Parser Text
dotAtom = fmap fold $ optional cfws *> many1 dotAtomText <* optional cfws

-- 3.2.4

qtext :: Parser Char
qtext = ranges [[33], [35 .. 91], [93 .. 126]] <|> obsQtext

qcontent :: Parser Text
qcontent = fmap one qtext <|> quotedpair

quotedstring :: Parser Text
quotedstring =
  optional cfws
    *> ( fold
          <$> sequence
            [ one <$> char '"'
            , fold <$> many (optional fws *> qcontent)
            , fmap one $ optional fws *> char '"'
            ]
       )
    <* optional cfws

-- 3.2.5

word :: Parser Text
word = atom <|> quotedstring

phrase :: Parser [Text]
phrase = many1 word <|> obsPhrase

unstructured :: Parser Text
unstructured = (tmany (optional fws *> vchar) <* many wsp) <|> obsUnstruct

-- 3.4

addresses :: Parser [Mailbox]
addresses = fmap one mailbox' <|> group

mailbox' :: Parser Mailbox
mailbox' = nameAddr <|> (Mailbox Nothing <$> addrSpec)

nameAddr :: Parser Mailbox
nameAddr = Mailbox <$> (T.unwords <<$>> optional phrase) <*> angleAddr

angleAddr :: Parser Email
angleAddr =
  ( optional cfws
      *> char '<'
      *> addrSpec
      <* char '>'
      <* optional cfws
  )
    <|> obsAngleAddr

group :: Parser [Mailbox]
group =
  displayName
    *> char ':'
    *> (optional groupList <&> (?: []))
    <* char ';'
    <* optional cfws

displayName :: Parser [Text]
displayName = phrase

mailboxList :: Parser (NonEmpty Mailbox)
mailboxList = liftA2 (:|) mailbox' (many (char ',' *> mailbox')) <|> obsMboxList

addressList :: Parser [Mailbox]
addressList =
  (fold <$> liftA2 (:) addresses (many (char ',' *> addresses))) <|> obsAddrList

groupList :: Parser [Mailbox]
groupList = fmap toList mailboxList <|> fmap (const []) (cfws <|> obsGroupList)

-- 3.4.1

addrSpec :: Parser Email
addrSpec = do
  l <- localpart

  -- Maximum length of local-part is 64, per RFC3696
  when (T.length l > 64) $
    fail "local-part of email is too long (more than 64 octets)"

  void (char '@') <?> "at sign"
  d <- domain

  -- Maximum length is 254, per Erratum 1690 on RFC3696
  when (T.length l + T.length d > 253) $
    fail "email address is too long (more than 254 octets)"

  pure $ Email l d

localpart :: Parser Text
localpart = dotAtom <|> quotedstring <|> obsLocalPart

domain :: Parser Text
domain = domainname <|> domainliteral <|> obsDomain

domainname :: Parser Text
domainname = do
  dom <- T.intercalate "." <$> domainlabel `sepEndBy1` string "."

  T.length dom <= 253 ?> dom

domainlabel :: Parser Text
domainlabel = do
  content <-
    optional cfws
      *> liftA2 (:|) alphaNum (many (alphaNum <|> char '-'))
      <* optional cfws

  length content <= 63 && last content /= '-' ?> toText (toList content)

domainliteral :: Parser Text
domainliteral =
  fmap fold $
    optional cfws
      *> char '['
      *> many (optional fws *> dtext)
      <* optional fws
      <* char ']'
      <* optional cfws

dtext :: Parser Text
dtext = fmap one (ranges [[33 .. 90], [94 .. 126]]) <|> obsDtext

obsPhrase :: Parser [Text]
obsPhrase = (:) <$> word <*> many (word <|> (one <$> char '.') <|> ("" <$ cfws))

-- OBSOLETE

-- 4.1

obsNoWsCtl :: Parser Char
obsNoWsCtl = ranges [[1 .. 8], [11, 12], [14 .. 31], [127]]

obsCtext, obsQtext, obsUtext :: Parser Char
obsCtext = obsNoWsCtl
obsQtext = obsNoWsCtl
obsUtext = char '\0' <|> obsNoWsCtl <|> vchar

obsQP :: Parser Text
obsQP = T.cons <$> char '\\' <*> fmap one (choice [nul, obsNoWsCtl, lf, cr])

obsUnstruct :: Parser Text
obsUnstruct =
  fold <<$>> many $
    (many lf *> many cr *> tmany (obsUtext <* many lf <* many cr))
      <|> ("" <$ fws)

-- 4.2

obsFws :: Parser ()
obsFws = void $ many1 wsp *> many (crlf *> many1 wsp)

-- 4.4

obsAngleAddr :: Parser Email
obsAngleAddr =
  optional cfws
    *> char '<'
    *> (obsRoute {- should be ignored -} *> addrSpec)
    <* char '>'
    <* optional cfws

obsRoute :: Parser [Text]
obsRoute = obsDomainList <* char ':'

obsDomainList :: Parser [Text]
obsDomainList = do
  void $ many (cfws <|> void (char ','))
  void $ char '@'
  dom <- domain
  doms <- many (char ',' *> optional cfws *> optional (char '@' *> domain))
  pure $ dom : catMaybes doms

obsMboxList :: Parser (NonEmpty Mailbox)
obsMboxList = do
  void . many $ optional cfws *> char ','
  mb <- mailbox'
  mbs <- many $ char ',' *> optional (fmap Just mailbox' <|> (Nothing <$ cfws))
  pure $ mb :| mapMaybe join mbs

obsAddrList :: Parser [Mailbox]
obsAddrList = do
  void . many $ optional cfws *> char ','
  mb <- addresses
  mbs <- many $ char ',' *> optional (fmap Just addresses <|> (Nothing <$ cfws))
  pure $ mb <> concatMap (fromMaybe [] . join) mbs

obsGroupList :: Parser ()
obsGroupList = void $ many1 (optional cfws *> char ',') *> optional cfws

obsLocalPart :: Parser Text
obsLocalPart = fmap fold $ (:) <$> word <*> many (T.cons <$> char '.' <*> word)

obsDomain :: Parser Text
obsDomain = fmap fold $ (:) <$> atom <*> many (T.cons <$> char '.' <*> atom)

obsDtext :: Parser Text
obsDtext = fmap one obsNoWsCtl <|> quotedpair
