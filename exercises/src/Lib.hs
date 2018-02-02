{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE Unsafe                #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeInType            #-}



module Lib
        ( 
          safeHead
        , safeTail
        , silly
        , toUnsafe
        , extractTail
        , createSafe
        , MarkedList (..)
        , SafeList (..)
        , safeHeadSafeList
        , safeTailSafeList
        , Door (..)
        , open
        , close
        , toggle
        , tryOpen
        , toggleDoors
        , toggleDoors'
        ) where

import Data.Kind 


-- 1

data SafeType = NotSafe | Safe

data MarkedList ::  * -> SafeType -> * where
    Nil :: MarkedList a 'NotSafe
    Cons :: a -> MarkedList a b -> MarkedList a с

instance Show a => Show (MarkedList a b) where
    show Nil = "Nil"
    show (Cons h t) = "Cons " ++ show h ++ " (" ++ show t ++ ")"


safeHead :: MarkedList a 'Safe -> a
safeHead (Cons h _) = h


toUnsafe :: forall a s . MarkedList a s -> MarkedList a 'NotSafe
toUnsafe Nil = Nil
toUnsafe (Cons h t) = ((Cons h t) :: MarkedList a 'NotSafe)

cons :: a -> MarkedList a s -> MarkedList a 'Safe
cons = Cons


extractTail :: MarkedList a 'Safe -> MarkedList a 'NotSafe
extractTail (Cons _ t) = toUnsafe t

safeTail :: MarkedList a 'Safe -> a
safeTail (Cons el Nil) = el
safeTail (Cons _ (Cons h t)) = safeTail $ cons h t

silly :: Bool -> MarkedList () 'NotSafe
silly False =  Nil
silly True =  Cons () Nil


data Empty
data NonEmpty

data SafeList a b where
     NilSafeList :: SafeList a Empty
     ConsSafeList:: a -> SafeList a b -> SafeList a NonEmpty

safeHeadSafeList :: SafeList a NonEmpty -> a
safeHeadSafeList (ConsSafeList x _) = x

safeTailSafeList :: SafeList a NonEmpty -> a
safeTailSafeList (ConsSafeList x NilSafeList) = x
safeTailSafeList (ConsSafeList _ l@ConsSafeList{}) = safeTailSafeList l

-- 2

createSafe :: a -> MarkedList a 'Safe
createSafe h = Cons h Nil

----Type Families

-- 4

type family And (a :: Bool) (b :: Bool) :: Bool where
    And 'True 'True = 'True
    And _ _ = 'False

type family Or (a :: Bool) (b :: Bool) :: Bool where
    Or 'False 'False = 'False
    Or _ _ = 'True

type family Not (a :: Bool) :: Bool where
    Not 'True = 'False
    Not _ = 'True

type family Implies (a :: Bool) (b :: Bool) :: Bool where -- используя другие type families, без паттерн-мэтчинга
    Implies a b = Not a `Or` b
    
type family If (a :: Bool) (ifTrue :: k) (ifFalse :: k) :: k where
    If 'True then' _ = then'
    If 'False _ else' = else'

-- :kind! If 'False 1 2 -> 2

-- 5

data Status = Open | Closed

data Door (status :: Status) where
    OpenDoor :: Door 'Open
    ClosedDoor :: Door 'Closed

open :: Door 'Closed -> Door 'Open
open _ = OpenDoor

close :: Door 'Open -> Door 'Closed
close _ = ClosedDoor

tryOpen :: Door a -> Door 'Open
tryOpen _ = OpenDoor


type family Toggle (s :: Status) where 
    Toggle 'Open = 'Closed
    Toggle 'Closed = 'Open

toggle :: Door a -> Door (Toggle a)
toggle OpenDoor = ClosedDoor
toggle ClosedDoor = OpenDoor

-- Есть ли какая-нибудь возможность написать функцию типа такой?
-- Тогда у toggleDoors можно сократить тип было бы до Door a -> Door (Toggle a) -> (Door (Toggle a), Door a)
-- toggle' :: Door (Toggle a) -> Door a
-- toggle' OpenDoor = ClosedDoor
-- toggle' ClosedDoor = OpenDoor

toggleDoors :: Door a -> Door (Toggle a) -> (Door (Toggle a), Door (Toggle (Toggle a)))
toggleDoors d1 d2 = (toggle d1, toggle d2)

toggleDoors' :: Door a -> Door (Toggle a) -> (Door (Toggle a), Door a)
toggleDoors' d1 d2 = (d2, d1)