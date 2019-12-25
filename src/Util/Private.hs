module Util.Private where

import Control.Applicative hiding (some, many)
import Text.Regex.Applicative hiding (some, many)
import Util hiding (some)

infixr 5 <:>, <++>
(<:>) :: (Applicative p, Alternative q) => p a -> p (q a) -> p (q a)
(<++>) :: (Applicative p, Alternative q) => p (q a) -> p (q a) -> p (q a)
(<:>) = liftA2 (<|)
(<++>) = liftA2 (<|>)

many :: Alternative q => RE x a -> RE x (q a)
many = reFoldl Greedy (|>) empty

some :: Alternative q => RE x a -> RE x (q a)
some a = a <:> many a
