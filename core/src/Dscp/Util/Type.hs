{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Type-level tricks.
module Dscp.Util.Type
    ( type (==)
    ) where

import Data.Type.Bool (type (&&))

-- | A type family to compute Boolean equality.
-- Poly-kinded version of (Data.Type.Equality.==), till the moment we switch to
-- lts-12.
type family (a :: k) == (b :: k) :: Bool where
  f a == g b = f == g && a == b
  a == a = 'True
  _ == _ = 'False
