module Util

------------------------------------------------------------------------

-- TODO It would be better if we could use Data.Bits but I couldn't get
-- TODO it working without the backend trying to compile 64-bit numbers, which
-- TODO VBA doesn't have.

class Bits a where
  or  : a -> a -> a
  and : a -> a -> a
  xor : a -> a -> a

------------------------------------------------------------------------
-- Bits8

instance Bits Bits8 where
    or  = prim__orB8
    and = prim__andB8
    xor = prim__xorB8

instance Cast Bits8 Bits32 where
    cast = prim__zextB8_B32

instance Cast Bits8 Int where
    cast = prim__zextB8_Int

instance Cast Bits8 Nat where
    cast = cast . cast {to = Int}

instance Cast Int Bits8 where
    cast = prim__truncInt_B8

instance Cast Nat Bits8 where
    cast = cast . cast {to = Int}

instance Enum Bits8 where
    pred    n = n - 1
    succ    n = n + 1
    toNat   n = cast n
    fromNat n = cast n
    enumFromTo n m =
      if n <= m
         then go [] (cast {to = Nat} (m - n)) m
         else []
         where
           go : List Bits8 -> Nat -> Bits8 -> List Bits8
           go acc Z     m = m :: acc
           go acc (S k) m = go (m :: acc) k (m - 1)

------------------------------------------------------------------------
-- Bits16

instance Bits Bits16 where
    or  = prim__orB16
    and = prim__andB16
    xor = prim__xorB16

instance Cast Bits16 Int where
    cast = prim__zextB16_Int

instance Cast Bits16 Nat where
    cast = cast . cast {to = Int}

instance Cast Int Bits16 where
    cast = prim__truncInt_B16

instance Cast Nat Bits16 where
    cast = cast . cast {to = Int}

instance Enum Bits16 where
    pred    n = n - 1
    succ    n = n + 1
    toNat   n = cast n
    fromNat n = cast n
    enumFromTo n m =
      if n <= m
         then go [] (cast {to = Nat} (m - n)) m
         else []
         where
           go : List Bits16 -> Nat -> Bits16 -> List Bits16
           go acc Z     m = m :: acc
           go acc (S k) m = go (m :: acc) k (m - 1)

------------------------------------------------------------------------
-- Bits32

instance Bits Bits32 where
    or  = prim__orB32
    and = prim__andB32
    xor = prim__xorB32

instance Cast Bits32 Int where
    cast = prim__zextB32_Int

instance Cast Bits32 Nat where
    cast = cast . cast {to = Int}

instance Cast Int Bits32 where
    cast = prim__truncInt_B32

instance Cast Nat Bits32 where
    cast = cast . cast {to = Int}

instance Enum Bits32 where
    pred    n = n - 1
    succ    n = n + 1
    toNat   n = cast n
    fromNat n = cast n
    enumFromTo n m =
      if n <= m
         then go [] (cast {to = Nat} (m - n)) m
         else []
         where
           go : List Bits32 -> Nat -> Bits32 -> List Bits32
           go acc Z     m = m :: acc
           go acc (S k) m = go (m :: acc) k (m - 1)
