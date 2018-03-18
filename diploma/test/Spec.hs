{-# OPTIONS_GHC -fplugin=Plugin #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ConstraintKinds    #-} -- Illegal constraint: c (Use ConstraintKinds to permit this) in Dict c


import Data.Typeable       (Proxy)
import Plugin              (Set)

data Dict c where
    Dict :: c => Dict c
  

-- test :: Proxy (Set '[Int, Bool, Int]) -> Proxy (Set '[Int, Bool])
-- test = id

test :: Proxy (Set '[Int]) -> Proxy (Set '[Int, Int])
test = id

-- proof1 :: Dict (Set '[Int, Bool, Bool] ~ Set '[Bool, Int]); proof1 = Dict

main :: IO ()
main = putStrLn "Test suite not yet implemented"
