module VBA.Memory

import VBA.Base
import Util

-- TODO Ideally we'd use the Idris effects library instead of this, but I
-- TODO didn't want to complicate things to much at this stage. We'd have to
-- TODO figure out how to mix and match different FFI nicely. Perhaps it's a
-- TODO case of having some kind of lifting operation to lift IO in to VBA.

------------------------------------------------------------------------
-- Allocation

malloc : Nat -> VBA Ptr
malloc sz = foreign FFI_VBA "libc.dylib/malloc" (Bits32 -> VBA Ptr) (cast sz)

calloc : Nat -> Nat -> VBA Ptr
calloc n sz = foreign FFI_VBA "libc.dylib/calloc" (Bits32 -> Bits32 -> VBA Ptr) (cast n) (cast sz)

free : Ptr -> VBA ()
free ptr = foreign FFI_VBA "libc.dylib/free" (Ptr -> VBA ()) ptr

------------------------------------------------------------------------
-- Peeking

peekBits8 : Ptr -> VBA Bits8
peekBits8 ptr = foreign FFI_VBA "prim$peekBits8" (Ptr -> VBA Bits8) ptr

peekBits16 : Ptr -> VBA Bits16
peekBits16 ptr = foreign FFI_VBA "prim$peekBits16" (Ptr -> VBA Bits16) ptr

peekBits32 : Ptr -> VBA Bits32
peekBits32 ptr = foreign FFI_VBA "prim$peekBits32" (Ptr -> VBA Bits32) ptr

peekCString : Ptr -> VBA String
peekCString ptr = foreign FFI_VBA "prim$peekCString" (Ptr -> VBA String) ptr

------------------------------------------------------------------------
-- Poking

pokeBits8 : Ptr -> Bits8 -> VBA ()
pokeBits8 ptr x = foreign FFI_VBA "prim$pokeBits8" (Ptr -> Bits8 -> VBA ()) ptr x

pokeBits16 : Ptr -> Bits16 -> VBA ()
pokeBits16 ptr x = foreign FFI_VBA "prim$pokeBits16" (Ptr -> Bits16 -> VBA ()) ptr x

pokeBits32 : Ptr -> Bits32 -> VBA ()
pokeBits32 ptr x = foreign FFI_VBA "prim$pokeBits32" (Ptr -> Bits32 -> VBA ()) ptr x

------------------------------------------------------------------------
-- Pointer Arithmetic

plusPtr : Ptr -> Nat -> Ptr
plusPtr ptr off = unsafePerformIO (foreign FFI_VBA "prim$plusPtr" (Ptr -> Bits32 -> VBA Ptr) ptr (cast off))
