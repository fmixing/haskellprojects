{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-} -- для ':
{-# LANGUAGE PolyKinds             #-} -- для "Unexpected kind variable ‘k’"
{-# LANGUAGE UndecidableInstances  #-}

module BasicTypeLevelSet where

import Lib (If)
import Data.Type.Equality

type family Insert (x :: k) (xs :: [k]) :: [k] where 
    Insert x xs = x ': xs

type family Lookup (x :: k) (xs :: [k]) :: Bool where 
    Lookup x '[] = 'False
    Lookup x (y : xs) = If (x == y) 'True (Lookup x xs)

type family Delete (x :: k) (xs :: [k]) :: [k] where 
    Delete x '[] = '[]
    Delete x (y : xs) = If (x == y) xs (y ': (Delete x xs))