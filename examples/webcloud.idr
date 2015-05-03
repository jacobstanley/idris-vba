module Main

import VBA

------------------------------------------------------------------------

printErrorCode : ErrorCode -> VBA ()
printErrorCode err = putStrLn ("Socket error: " ++ show err)

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

  ve_handle printErrorCode $ do
    sock <- socket AF_INET Stream 0
    ve_lift (print sock)

    setNonBlocking sock
    ve_lift (putStrLn "Set to non-blocking")

    bind sock 8080
    ve_lift (putStrLn "Bound to port 8080")

    listen sock 64
    ve_lift (putStrLn "Listening on port 8080")

    m <- accept sock
    case m of
      Nothing => ve_lift (putStrLn "Did not accept client")
      Just (csock, caddr) => do
        ve_lift (putStrLn $ "Accepted client " ++ show caddr)
        close csock
        ve_lift (putStrLn "Client socket closed")

    close sock
    ve_lift (putStrLn "Socket closed")
