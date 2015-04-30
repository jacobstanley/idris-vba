module VBA

------------------------------------------------------------------------

-- Supported VBA foreign types
data VBA_Types : Type -> Type where
     VBA_Boolean  : VBA_Types Bool
     VBA_Byte     : VBA_Types Bits8
     VBA_Long     : VBA_Types Int
     VBA_Double   : VBA_Types Float
     VBA_String   : VBA_Types String
     VBA_Unit     : VBA_Types ()

FFI_VBA : FFI
FFI_VBA = MkFFI VBA_Types String String

VBA : Type -> Type
VBA a = IO' FFI_VBA a

------------------------------------------------------------------------

mid : String -> Int -> Int -> String
mid s i l = unsafePerformIO (foreign FFI_VBA "Mid" (String -> Int -> Int -> VBA String) s i l)

------------------------------------------------------------------------

clearCells : VBA ()
clearCells = foreign FFI_VBA "Cells.Delete" (VBA ())

putCell : Int -> Int -> String -> VBA ()
putCell x y str = foreign FFI_VBA "Cells(%0,%1)=%2" (Int -> Int -> String -> VBA ()) x y str
