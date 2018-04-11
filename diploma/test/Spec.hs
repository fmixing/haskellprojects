{-# OPTIONS_GHC -fplugin=Plugin #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ConstraintKinds    #-} -- Illegal constraint: c (Use ConstraintKinds to permit this) in Dict c
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE TypeInType         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}



import Data.Typeable       (Proxy)
import Plugin              (Set)
import GHC.TypeLits        (KnownNat, Nat, type (+), type (-), natVal, CmpNat, someNatVal, sameNat, SomeNat(SomeNat))
import Data.Kind

type family Apart a b where
    Apart a a = False
    Apart a b = True
  
type a /~ b = Apart a b ~ True

data Dict c where
    Dict :: c => Dict c

-- ProxiesAscN n = '[Proxy 1, Proxy 2, ..., Proxy n]
-- ProxiesDescN n = '[Proxy n, Proxy n - 1, ... , Proxy 1]

-- Set (ProxiesAscN n) ~ Set (ProxiesDescN n)

type family ProxiesAscN (n :: Nat) :: [*] where 
    ProxiesAscN k = ProxyAscNHelper k 1

type family ProxiesDescN (n :: Nat) :: [*] where 
    ProxiesDescN 0 = '[]
    ProxiesDescN k = (Proxy k) ': (ProxiesDescN (k - 1))

type family ProxyAscNHelper (n :: Nat) (m :: Nat) :: [*] where
    ProxyAscNHelper 0 k = '[]
    ProxyAscNHelper n k = (Proxy k) ': (ProxyAscNHelper (n - 1) (k + 1))

test :: Proxy (Set '[Int, Bool, Int]) -> Proxy (Set '[Bool, Int])
test = id

test1 :: Proxy (Set '[Int]) -> Proxy (Set '[Int, Int])
test1 = id

-- test2 :: Proxy (Set '[Int, Bool, Int]) -> Proxy (Set '[Int])
-- test2 = id

test3 :: Proxy (Set '[[Char], [Char]]) -> Proxy (Set '[[Char]])
test3 = id


-- test4 :: Proxy (Set '[Proxy 3, Proxy 1, Proxy 5, Proxy 10, Proxy 7]) -> Proxy (Set '[Proxy 10, Proxy 7, Proxy 3, Proxy 1, Proxy 5])
-- test4 = id

test5 :: Proxy (Set (ProxiesAscN 100)) -> Proxy (Set (ProxiesDescN 100))
test5 = id

-- proof1 :: Dict (Set '[Int, Bool, Bool] ~ Set '[Bool, Int]); proof1 = Dict

-- proof2 :: Dict (Set '[Int, Bool] ~ Set '[Bool]); proof2 = Dict

-- proof2 :: Dict ((Set '[Int, Bool]) /~ (Set '[Bool])); proof2 = Dict

main :: IO ()
main = putStrLn "Test suite not yet implemented"
