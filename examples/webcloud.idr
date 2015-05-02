module Main

import VBA
import Socket

------------------------------------------------------------------------

printError : SocketError -> VBA ()
printError err = putStrLn ("Socket error: " ++ show err)

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

  case !(socket AF_INET Stream 0) of
    Left  err  => printError err
    Right sock => do
      print sock
      case !(bind sock 8080) of
        Nothing  => putStrLn "Bound to port 8080"
        Just err => printError err
      close sock
      putStrLn "Socket Closed"
