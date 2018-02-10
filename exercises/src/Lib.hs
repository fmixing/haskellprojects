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
        , openWithCondition
        , Sort
        , Sort'
        , If
        , (++)
        ) where

-- а как импортнуть (*) явно?
import Data.Kind 
import GHC.TypeLits (Symbol, CmpSymbol)

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

data TBool (p :: Bool) where
    TTrue :: TBool 'True
    TFalse :: TBool 'False

type family OpenWithCondition (b :: Bool) (s :: Status) where 
    OpenWithCondition 'False 'Closed = 'Closed    
    OpenWithCondition _ _ = 'Open

--Не совсем поняла, что делать в таком случае: мне пришлось явно паттерн-матчиться, потому что если проверить
--только первое условие, а в остальных остаить _ _, компилятор вроде не может применить OpenWithCondition.
--Есть ли какой-нибудь способ не делать этого?
openWithCondition :: forall s p . Door s -> TBool p -> Door (OpenWithCondition p s)
openWithCondition ClosedDoor TFalse = ClosedDoor
openWithCondition ClosedDoor TTrue = OpenDoor
openWithCondition OpenDoor TTrue = OpenDoor
openWithCondition OpenDoor TFalse = OpenDoor


-- 6

type family Less (o :: Ordering) :: Bool where
    Less 'LT = 'True
    Less _ = 'False


type family (++) (as :: [k]) (bs :: [k]) :: [k] where
  (++) a '[] = a
  (++) '[] b = b
  (++) (a ': as) bs = a ': (as ++ bs)


--Есть ли возможность написать что-то типа let?
type family Sort (xs :: [Symbol]) :: [Symbol] where 
    Sort ('[]) = '[]
    Sort (x ': '[]) = '[x]
    Sort (x : xs) = Sort (Fst (Split x xs)) ++ (x ': Sort (Snd (Split x xs)))

type family Split (x :: Symbol) (xs :: [Symbol]) :: ([Symbol], [Symbol]) where
    Split x ('[]) = '( '[], '[])
    Split x (y : ys) = If (Less (CmpSymbol y x)) 
        '(y : (Fst (Split x ys)), (Snd (Split x ys))) '((Fst (Split x ys)), y : (Snd (Split x ys)))

type family Fst (pair :: ([Symbol], [Symbol])) :: [Symbol] where
    Fst '(xs, ys) = xs

type family Snd (pair :: ([Symbol], [Symbol])) :: [Symbol] where
    Snd '(xs, ys) = ys



type family Sort' (xs :: [Symbol]) :: [Symbol] where 
    Sort' ('[]) = '[]
    Sort' (x ': '[]) = '[x]
    Sort' (x : xs) = Sort (LessElems x xs) ++ (x ': Sort (GreaterOrEqElems x xs))

type family LessElems (x :: Symbol) (xs :: [Symbol]) :: [Symbol] where
    LessElems x '[] = '[]
    LessElems x (y : ys) = If (Less (CmpSymbol y x)) (y : LessElems x ys) (LessElems x ys)

type family GreaterOrEqElems (x :: Symbol) (xs :: [Symbol]) :: [Symbol] where
    GreaterOrEqElems x '[] = '[]    
    GreaterOrEqElems x (y : ys) = If (Less (CmpSymbol y x)) (GreaterOrEqElems x ys) (y : GreaterOrEqElems x ys)