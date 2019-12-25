{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
module Text.Regex.Applicative.Lex (natural', natural, ident', ident, IdentSpec (..), defaultIdentSpec, string, esc, unicodepoint) where

import           Control.Applicative.Combinators (between, count')
import           Control.Monad ((>=>), guard, replicateM)
import qualified Data.Char.Properties.DerivedCore as UC
import qualified Data.DList as DList
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Normalize as T
import           Numeric.Natural
import           Text.Regex.Applicative hiding (string, some, many)
import qualified Text.Regex.Applicative as RE
import           Util hiding (some)
import           Util.Private

natural' :: RE Char Natural
natural' = asum
  [ natural 10
  , traverse sym "0b" *> natural  2
  , traverse sym "0o" *> natural  8
  , traverse sym "0d" *> natural 10
  , traverse sym "0x" *> natural 16
  ]

natural :: Word -> RE Char Natural
natural = naturalWith some

naturalWith :: (RE Char Word -> RE Char [Word]) -> Word -> RE Char Natural
naturalWith f r = fromDigits r <$> f (digit' r)

fromDigits :: Integral a => Word -> [a] -> Natural
fromDigits r = foldl' (\ n w -> fromIntegral r*n + fromIntegral w) 0

digit' :: Word -> RE Char Word
digit' r = msym (digit >=> \ n -> n <$ guard (n < r))

ident' :: RE Char Text
ident' = ident defaultIdentSpec

ident :: IdentSpec -> RE Char Text
ident IdentSpec {..} = normalize . T.pack . DList.toList <$>
    start <:> many cont <++> (fmap asum . RE.many) (med <:> some cont)
  where
    start = psym isStart
    cont = psym isContinue
    med = psym isMedial

data IdentSpec = IdentSpec
  { isStart, isContinue, isMedial :: Char -> Bool
  , normalize :: Text -> Text }

defaultIdentSpec :: IdentSpec
defaultIdentSpec = IdentSpec
  { isStart = UC.isXIDStart <||> (∈ "_")
  , isContinue = UC.isXIDContinue <&&> (∉ "·\x0387") <||> (∈ "_\x05F3")
  , isMedial = (∈ "'-·\x0387\x058A\x05F4\x0F0B\x2010\x2027\x30A0\x30FB")
  , normalize = T.map normalizeIDMedial . T.normalize T.NFC
  } where
    normalizeIDMedial = \ case
        '\x058A' -> '-'
        '\x2010' -> '-'
        '\x30A0' -> '-'
        '\x0F0B' -> '·'
        '\x2027' -> '·'
        '\x30FB' -> '·'
        x       -> x

string :: RE Char TL.Text
string = TL.pack . DList.toList <$> between (sym '"') (sym '"') (many xre)
  where xre = psym (∉ "\"\\") <|> sym '\\' *> (psym (∈ "\"\\") <|> esc)

esc :: RE Char Char
esc = asum
    [toEnum . fromIntegral <$ sym 'x' <*> naturalWith (replicateM 2) 16
    , sym 'u' *> between (sym '{') (sym '}') unicodepoint
    , '\x00' <$ sym '0'
    , '\x07' <$ sym 'a'
    , '\x08' <$ sym 'b'
    , '\x09' <$ sym 't'
    , '\x0A' <$ sym 'n'
    , '\x0B' <$ sym 'v'
    , '\x0C' <$ sym 'f'
    , '\x0D' <$ sym 'r'
    , '\x1B' <$ sym 'e'
    ]

unicodepoint :: RE Char Char
unicodepoint = (\ a b -> toEnum . fromIntegral $ 0x10000 * a + b)
    <$> asum [16 <$ RE.string "10", naturalWith (fmap pure) 16]
    <*> naturalWith (replicateM 4) 16
    <|> toEnum . fromIntegral <$> naturalWith (count' 0 4) 16
