{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
module Text.Lexer (Error (..), LexerSpec (token, space, blockComment, move, init), defaultLexerSpec, lex) where

import           Prelude hiding (lex)
import           Control.Applicative
import           Control.Monad.Free (Free (..))
import qualified Data.Char as Char
import           Data.Semigroup (Min (..), Max (..))
import           Text.Regex.Applicative (RE, findLongestPrefixWithUncons)
import qualified Text.Regex.Applicative as RE
import           Util

data LexerSpec p x t = LexerSpec
  { token :: RE x t
  , space :: RE x ()
  , blockComment :: RE x (RE x ())
  , move :: x -> p -> p
  , init :: p
  }

defaultLexerSpec :: Num n => LexerSpec n Char t
defaultLexerSpec = LexerSpec
  { token = empty
  , space = () <$ many (RE.psym Char.isSpace)
  , blockComment = empty
  , move = pure (+1)
  , init = 0
  }

lex :: LexerSpec p x t -> [x] -> Free ((,,) (Min p, Max p) t) (Maybe (Error p))
lex LexerSpec {..} = go' [] . annotate move init
  where
    go bs = stripLongestPrefixWithUncons stripAnnotation (many space) & go' bs
    go' bs' xs@(AList p l) = case (bs', l) of
        ([], Nothing) -> Pure Nothing
        (_,  Nothing) -> Pure (Just (Error p))
        (b:bs, Just (_, xs')) -> case findLongestPrefixWithUncons stripAnnotation (Nothing <$ b <|> Just <$> blockComment) xs of
            Just (Nothing, xs) -> (case bs of [] -> go; _ -> go') bs xs
            Just (Just b', xs) -> go' (b':bs') xs
            Nothing -> go' bs' xs'
        ([], Just _) -> case findLongestPrefixWithUncons stripAnnotation' (Left <$> blockComment <|> Right <$> token) (xs, p) of
            Just (Left b, (xs, _)) -> go' [b] xs
            Just (Right t, (xs, q)) -> Free ((Min p, Max q), t, go [] xs)
            Nothing -> Pure (Just (Error p))

data AList a b = AList { annotation :: a, stripAnnotation :: Maybe (b, AList a b) }
  deriving (Eq, Show)

stripAnnotation' :: (AList a b, a) -> Maybe (b, (AList a b, a))
stripAnnotation' = fst & \ (AList a l) -> fmap (flip (,) a) <$> l

annotate :: (a -> b -> b) -> b -> [a] -> AList b a
annotate f = go
  where
    go b = AList b . \ case
        [] -> Nothing
        a:as -> let b' = f a b in b' `seq` Just (a, go b' as)

newtype Error p = Error { errorPos :: p }
  deriving (Eq, Ord, Read, Show)

stripLongestPrefixWithUncons :: (xs -> Maybe (x, xs)) -> RE x a -> xs -> xs
stripLongestPrefixWithUncons uncons re = flip maybe snd <*> findLongestPrefixWithUncons uncons re
