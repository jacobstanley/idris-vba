Idris VBA back end
------------------

Based on Edwin Brady's PHP backend.

Generates .bas files which can be imported in to Microsoft Excel as a
module.

## Does this really work?

Yes! But it is not a complete backend by any means, there are many
situations which will not work just yet.

## Interesting Features

- Unsigned numbers
- Foreign calls to VBA
- Foreign calls to C libraries
- General tail call elimination

## Example

See [generated
code](https://github.com/jystic/idris-vba/blob/master/examples/pythag.bas).

```idris
module Main

import VBA
import Util

------------------------------------------------------------------------

zipIx : Int -> List a -> List (Int, a)
zipIx _ []        = []
zipIx n (x :: xs) = (n, x) :: zipIx (n+1) xs

putResult : (Int, Int, Int, Int) -> VBA ()
putResult (i, x, y, z) = do
  putCell i 1 (show x)
  putCell i 2 (show y)
  putCell i 3 (show z)

------------------------------------------------------------------------

pythag : Int -> List (Int, Int, Int)
pythag max = [(x, y, z) | z <- [1..max], y <- [1..z], x <- [1..y],
                          x * x + y * y == z * z ]

main : VBA ()
main = do
  putStrLn "Clearing cells..."
  clearCells

  putStrLn "Writing headers..."
  putCell 1 1 "X"
  putCell 1 2 "Y"
  putCell 1 3 "Z"

  putStrLn "Calculating..."
  mapM_ putResult $ zipIx 2 (pythag 50)
```

![screenshot](https://github.com/jystic/idris-vba/raw/master/screenshot.png)

## Foreign Function Interface

The VBA backend has full support for accessing both VBA and C foreign
functions.

You can bind simple VBA functions using the standard FFI style:

```idris
mid : String -> Int -> Int -> VBA String
mid s i l = foreign FFI_VBA "Mid" (String -> Int -> Int -> VBA String) s i l
```

Or property assignment using the indexed FFI style:

```idris
putCell : Int -> Int -> String -> VBA ()
putCell x y str = foreign FFI_VBA "Cells(%0,%1)=%2" (Int -> Int -> String -> VBA ()) x y str
```

To bind C functions, you need to specify the library which contains the
function as well as the function name itself, separated by a `/`.

```idris
htonl : Bits32 -> VBA Bits32
htonl x = foreign FFI_VBA "libc.dylib/htonl" (Bits32 -> VBA Bits32) x
```
