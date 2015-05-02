module Util

------------------------------------------------------------------------

mapM_ : Monad m => (a -> m ()) -> List a -> m ()
mapM_ _ []        = return ()
mapM_ f (x :: xs) = f x >>= \_ => mapM_ f xs

------------------------------------------------------------------------
-- Bits8

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
