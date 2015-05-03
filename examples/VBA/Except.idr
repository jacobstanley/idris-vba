module VBA.Except

import VBA.Base

------------------------------------------------------------------------
-- VBA monad with exception handling (same as IOExcept)

data VBAExcept : Type -> Type -> Type where
     vbaM : VBA (Either err a) -> VBAExcept err a

instance Functor (VBAExcept e) where
     map f (vbaM fn) = vbaM (map (map f) fn)

instance Applicative (VBAExcept e) where
     pure x = vbaM (pure (pure x))
     (vbaM f) <*> (vbaM a) = vbaM (do f' <- f; a' <- a
                                      return (f' <*> a'))
instance Monad (VBAExcept e) where
     (vbaM x) >>= k = vbaM (do x' <- x;
                               case x' of
                                    Right a  => let vbaM ka = k a in ka
                                    Left err => return (Left err))

ve_lift : VBA a -> VBAExcept err a
ve_lift op = vbaM (do op' <- op
                      return (Right op'))

ve_fail : err -> VBAExcept err a
ve_fail e = vbaM (return (Left e))

ve_run : VBAExcept err a -> (err -> VBA b) -> (a -> VBA b) -> VBA b
ve_run (vbaM act) err ok = do act' <- act
                              case act' of
                                   Left e  => err e
                                   Right v => ok v

ve_handle : (err -> VBA a) -> VBAExcept err a -> VBA a
ve_handle h ve = ve_run ve h return
