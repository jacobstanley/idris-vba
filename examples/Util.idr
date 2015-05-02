module Util

------------------------------------------------------------------------

mapM_ : Monad m => (a -> m ()) -> List a -> m ()
mapM_ _ []        = return ()
mapM_ f (x :: xs) = f x >>= \_ => mapM_ f xs

------------------------------------------------------------------------

instance Enum Bits8 where
    pred    n = n - 1
    succ    n = n + 1
    toNat   n = cast (prim__zextB8_Int n)
    fromNat n = prim__truncInt_B8 (cast n)

instance Enum Bits16 where
    pred    n = n - 1
    succ    n = n + 1
    toNat   n = cast (prim__zextB16_Int n)
    fromNat n = prim__truncInt_B16 (cast n)
