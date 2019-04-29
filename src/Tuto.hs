{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Tuto where

import           Data.Kind    (Type)
import           GHC.TypeLits

-- data Nat :: * where
--   Z :: Nat
--   S :: Nat -> Nat

-- data Vector :: Nat -> Type -> Type where
--   VNil :: Vector 'Z a
--   VCons :: a -> Vector n a -> Vector ('S n) a

-- data Vector :: Nat -> Type -> Type where
--   VNil :: Vector 0 a
--   VCons :: a -> Vector n a -> Vector (n + 1) a

data Vector (n :: Nat) (a :: Type) where
  VNil :: Vector 0 a
  (:::) :: Show a => a -> Vector n a -> Vector (n + 1) a

deriving instance Show (Vector n a)

infixr 6 :::

vector123 :: Vector 3 Int
vector123 = 1 ::: 2 ::: 3 ::: VNil

vhead :: Vector (1 + n) a -> a
vhead (head ::: tail) = head
