{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
module Text.Regex.Applicative.Lex (natural', natural, ident', ident, IdentSpec (..), defaultIdentSpec) where

import           Control.Monad ((>=>), guard)
import qualified Data.Char.Properties.DerivedCore as UC
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Normalize as T
import           Numeric.Natural
import           Text.Regex.Applicative
import           Util hiding (some)

natural' :: RE Char Natural
natural' = asum
  [ natural 10
  , traverse sym "0b" *> natural  2
  , traverse sym "0o" *> natural  8
  , traverse sym "0d" *> natural 10
  , traverse sym "0x" *> natural 16
  ]

natural :: Word -> RE Char Natural
natural r = foldl' (\ n w -> fromIntegral r*n + fromIntegral w) 0 <$> some (msym (digit >=> \ n -> n <$ guard (n < r)))

ident' :: RE Char Text
ident' = ident defaultIdentSpec

ident :: IdentSpec -> RE Char Text
ident IdentSpec {..} = normalize . T.pack <$> start <:> many cont <++> (fmap asum . many) (med <:> some cont)
  where
    start = psym isStart
    cont = psym isContinue
    med = psym isMedial
    infixr 5 <:>, <++>
    (<:>) = liftA2 (:)
    (<++>) = liftA2 (++)

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
