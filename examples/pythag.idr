module Main

import VBA

------------------------------------------------------------------------

mapM_ : (a -> VBA ()) -> List a -> VBA ()
mapM_ _ []        = return ()
mapM_ f (x :: xs) = f x >>= \_ => mapM_ f xs

zipIx : Int -> List a -> List (Int, a)
zipIx _ []        = []
zipIx n (x :: xs) = (n, x) :: zipIx (n+1) xs

------------------------------------------------------------------------

pythag : Int -> List (Int, Int, Int)
pythag max = [(x, y, z) | z <- [1..max], y <- [1..z], x <- [1..y],
                          x * x + y * y == z * z ]

putResult : (Int, Int, Int, Int) -> VBA ()
putResult (i, x, y, z) = do
  putCell i 1 (show x)
  putCell i 2 (show y)
  putCell i 3 (show z)

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
