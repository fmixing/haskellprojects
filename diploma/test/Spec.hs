{-# OPTIONS_GHC -fplugin=Plugin #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ConstraintKinds    #-} -- Illegal constraint: c (Use ConstraintKinds to permit this) in Dict c
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE PolyKinds          #-}

import Data.Typeable       (Proxy)
import Plugin              (Set)

type family Apart a b where
    Apart a a = False
    Apart a b = True
  
type a /~ b = Apart a b ~ True

data Dict c where
    Dict :: c => Dict c
  

-- test :: Proxy (Set '[Int, Bool, Int]) -> Proxy (Set '[Bool, Int])
-- test = id

test1 :: Proxy (Set '[Int]) -> Proxy (Set '[Int, Int])
test1 = id

test2 :: Proxy (Set '[Int, Bool, Int]) -> Proxy (Set '[Int])
test2 = id

-- proof1 :: Dict (Set '[Int, Bool, Bool] ~ Set '[Bool, Int]); proof1 = Dict

-- proof2 :: Dict (Set '[Int, Bool] ~ Set '[Bool]); proof2 = Dict

-- proof2 :: Dict ((Set '[Int, Bool]) /~ (Set '[Bool])); proof2 = Dict

main :: IO ()
main = putStrLn "Test suite not yet implemented"
