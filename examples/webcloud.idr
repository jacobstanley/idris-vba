module Main

import VBA
import Util

------------------------------------------------------------------------

main : VBA ()
main = do
  let h16 : Bits16 = 0x1234
  let h32 : Bits32 = 0x1234ABCD

  putStrLn $ "h16 = " ++ show h16
  putStrLn $ "h32 = " ++ show h32

  let n16 = htons h16
  let n32 = htonl h32

  putStrLn $ "n16 = " ++ show n16
  putStrLn $ "n32 = " ++ show n32
