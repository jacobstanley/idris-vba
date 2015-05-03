module VBA.Excel

import VBA.Base

------------------------------------------------------------------------
-- Excel

clearCells : VBA ()
clearCells = foreign FFI_VBA "Cells.Delete" (VBA ())

putCell : Int -> Int -> String -> VBA ()
putCell x y str = foreign FFI_VBA "Cells(%0,%1)=%2" (Int -> Int -> String -> VBA ()) x y str
