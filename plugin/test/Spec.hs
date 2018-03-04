{-# OPTIONS_GHC -fplugin=OClockPlugin #-}
{-# LANGUAGE TemplateHaskell, ExplicitNamespaces, DataKinds, TypeOperators, KindSignatures, GADTs, TypeInType, 
    RankNTypes, StandaloneDeriving, ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}


import Time.Rational
import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(Proxy))

-- data Test :: Rat -> Constraint where
--     TestInst :: Test (DivRat (9 % 11) (9 % 11))


-- test :: (KnownRat a, KnownRat b) => Proxy (DivRat a b) -> RatioNat
-- test div = ratVal div

test :: forall a b . (KnownRat a, KnownRat b) => RatioNat
test = ratVal @(DivRat a b)


main :: IO ()
main = test (Proxy :: DivRat (9 % 11) (9 % 11))
