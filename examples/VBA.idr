module VBA

------------------------------------------------------------------------

data ByRef : Type -> Type where
     MkRef : (x : t) -> ByRef t

%used MkRef x

-- Supported VBA foreign types
data VBA_Types : Type -> Type where
     VBA_Bool   : VBA_Types Bool   -- Boolean
     VBA_Bits8  : VBA_Types Bits8  -- Byte
     VBA_Bits16 : VBA_Types Bits16 -- Integer
     VBA_Bits32 : VBA_Types Bits32 -- Long
     VBA_Int    : VBA_Types Int    -- Long
     VBA_Float  : VBA_Types Float  -- Double
     VBA_String : VBA_Types String -- String
     VBA_Ptr    : VBA_Types Ptr
     VBA_Unit   : VBA_Types ()
     VBA_ByRef  : VBA_Types a -> VBA_Types (ByRef a)

FFI_VBA : FFI
FFI_VBA = MkFFI VBA_Types String String

VBA : Type -> Type
VBA a = IO' FFI_VBA a

------------------------------------------------------------------------
-- VBA

mid : String -> Int -> Int -> String
mid s i l = unsafePerformIO (foreign FFI_VBA "Mid" (String -> Int -> Int -> VBA String) s i l)

------------------------------------------------------------------------
-- Excel

clearCells : VBA ()
clearCells = foreign FFI_VBA "Cells.Delete" (VBA ())

putCell : Int -> Int -> String -> VBA ()
putCell x y str = foreign FFI_VBA "Cells(%0,%1)=%2" (Int -> Int -> String -> VBA ()) x y str
