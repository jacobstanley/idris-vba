module Main

import VBA
import Util

------------------------------------------------------------------------

main : VBA ()
main = do
  let b8s  : List Bits8  = [0x00..0xFF]
  let b16s : List Bits16 = [0xFF00..0xFF0F]

  let b8  : Bits8  = 0xFF
  let b16 : Bits16 = 0xFFFF

  print b8s
  print b8
  print b16

  mapM_ print b16s

  --let str = mid "Hello World!" 7 5
  --print str
  --putCell 1 1 str
