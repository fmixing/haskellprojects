{-# OPTIONS_GHC -fplugin=OClockPlugin #-}
{-# LANGUAGE TemplateHaskell, ExplicitNamespaces, DataKinds, TypeOperators, KindSignatures, GADTs, TypeInType, 
    RankNTypes, StandaloneDeriving, ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}


import Time.Rational
import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(Proxy))

testOk :: forall a b . (KnownRat a, KnownRat b) => RatioNat
testOk = ratVal @(DivRat a b)

-- testBad :: forall a b . (KnownRat b) => RatioNat
-- testBad = ratVal @(DivRat a b)


main :: IO ()
main = do
    print $ testOk @(5 / 1) @(7 / 1)
    -- print $ testBad @(5 / 1) @(7 / 1)
