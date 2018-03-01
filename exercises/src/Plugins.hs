{-# LANGUAGE TemplateHaskell, ExplicitNamespaces, DataKinds, TypeOperators, KindSignatures, GADTs, TypeInType, RankNTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-} --Без него не могли сойтись типы у proxyEq2, example
{-# OPTIONS_GHC -O -fplugin Test.Inspection.Plugin #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin Data.UnitsOfMeasure.Plugin #-}


module Plugins where

import Test.Inspection
import Data.Maybe
import GHC.TypeLits (KnownNat, Nat, type (+), type (-), type (^), natVal, CmpNat)
import qualified GHC.TypeLits as TL (type (*))
import Data.Kind 
import Data.Proxy
import Data.Type.Equality
import Data.UnitsOfMeasure



lhs, rhs :: (a -> b) -> Maybe a -> Bool
lhs f x = isNothing (fmap f x)
rhs f Nothing = True
rhs f (Just _) = False

-- inspect $ 'lhs === 'rhs

-- :kind! 0 + 1
-- 0 + 1 :: Nat
-- = 1

data Vec :: Nat -> * -> * where
    Nil  :: Vec 0 a
    Succ :: a -> Vec (n - 1) a -> Vec n a

instance Show a => Show (Vec b a) where
    show Nil = "Nil"
    show (Succ h t) = "Cons " ++ show h ++ " (" ++ show t ++ ")"
    
--Без -fplugin GHC.TypeLits.Normalise выдает ошибку: 
-- Couldn't match type ‘(n - 1) + m’ with ‘(n + m) - 1’
-- Expected type: Vec ((n + m) - 1) a
--   Actual type: Vec ((n - 1) + m) a
append :: Vec n a -> Vec m a -> Vec (n + m) a
append Nil vec = vec
append (Succ x vec1) vec2 = Succ x (append vec1 vec2)


proxyEq1 :: Proxy ((2 ^ x) TL.* (2 ^ (x + x))) -> Proxy (2 TL.* (2 ^ ((x + (x + x)) - 1)))
proxyEq1 = id

proxyEq2 :: Proxy ((x + 2)^(y + 2)) -> Proxy (4 TL.* x TL.* (2 + x)^y + 4 TL.* (2 + x)^y + (2 + x)^y TL.* x^2)
proxyEq2 = id

example :: (x + 2)^(y + 2) :~: 4 TL.* x TL.* (2 + x)^y + 4 TL.* (2 + x)^y + (2 + x)^y TL.* x^2
example = Refl
