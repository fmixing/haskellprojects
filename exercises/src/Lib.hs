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



module Lib
        ( 
          safeHead
        , safeTail
        , silly
        , toUnsafe
        , extractTail
        , MarkedList (..)
        , SafeList (..)
        , safeHeadSafeList
        , safeTailSafeList
        ) where

-- 1

data NotSafe
data Safe

data MarkedList ::  * -> * -> * where
    Nil :: MarkedList a NotSafe
    Cons :: a -> MarkedList a b -> MarkedList a с

instance Show a => Show (MarkedList a b) where
    show Nil = "Nil"
    show (Cons h t) = "Cons " ++ show h ++ " (" ++ show t ++ ")"


safeHead :: MarkedList a Safe -> a
safeHead (Cons h _) = h


toUnsafe :: forall a . (forall s . MarkedList a s -> MarkedList a NotSafe)
toUnsafe Nil = Nil
toUnsafe (Cons h t) = ((Cons h t) :: MarkedList a NotSafe)

toSafe :: forall a . (forall s . MarkedList a s -> MarkedList a Safe)
toSafe Nil = createSafe
toSafe (Cons h t) = (Cons h t :: MarkedList a Safe)


extractTail :: MarkedList a Safe -> MarkedList a NotSafe
extractTail (Cons _ t) = toUnsafe t

safeTail :: MarkedList a Safe -> a
safeTail (Cons el Nil) = el
safeTail (Cons _ list@Cons{}) = safeTail $ toSafe list

silly :: Bool -> MarkedList () NotSafe
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

createSafe :: MarkedList a Safe
createSafe = Cons undefined Nil


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

