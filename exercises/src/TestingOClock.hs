{-# LANGUAGE TemplateHaskell, ExplicitNamespaces, DataKinds, TypeOperators, KindSignatures, GADTs, TypeInType, 
    RankNTypes, StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-} -- Scoped Type Variables are an extension to Haskell's type system 
                                     -- that allow free type variables to be re-used in the scope of a function.
                                     -- Без этого не работает natVal (Proxy :: Proxy n)
                                     -- и natVal @n Proxy

{-# LANGUAGE TypeApplications #-} -- возможность написать five
{-# LANGUAGE AllowAmbiguousTypes #-} -- 


module TestingOClock where

import GHC.TypeLits (KnownNat, Nat, type (+), type (-), type (^), natVal, CmpNat, someNatVal, sameNat, SomeNat(SomeNat))
import Data.Maybe (isJust)
import Data.Proxy (Proxy(Proxy))
import Data.Kind (Type)


-- type E = (5 :: Nat)
-- p = (Proxy :: Proxy 5), natVal p
-- natVal (Proxy :: Proxy 5)

data Vec :: Nat -> Type -> Type where 
    Nil :: Vec 0 a
    Succ :: a -> Vec n a -> Vec (n + 1) a

deriving instance Show a => Show (Vec len a)

-- p = (Proxy :: Proxy 5)
-- test p
test :: forall n . KnownNat n => Proxy n -> Integer
test = natVal

length :: forall n a . KnownNat n => Vec n a -> Integer
length _ = natVal (Proxy :: Proxy n)


-- isLength :: forall len a. KnownNat len => Integer -> Vec len a -> Bool
-- isLength n _ | Just (SomeNat p) <- someNatVal n = isJust (sameNat (Proxy :: Proxy len) p)
--              | otherwise = False

ambiguousNatVal :: forall n . (KnownNat n) => Integer
ambiguousNatVal = natVal @n Proxy

five = ambiguousNatVal @5 -- no `Proxy ` needed!