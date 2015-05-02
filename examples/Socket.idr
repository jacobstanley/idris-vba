module Socket

import VBA

------------------------------------------------------------------------

%access public

ByteLength : Type
ByteLength = Int

class ToCode a where
  toCode : a -> Bits32

------------------------------------------------------------------------

||| Socket Families
data SocketFamily =
  ||| Unspecified
  AF_UNSPEC |
  ||| IP / UDP etc. IPv4
  AF_INET |
  |||  IP / UDP etc. IPv6
  AF_INET6

instance Show SocketFamily where
  show AF_UNSPEC = "AF_UNSPEC"
  show AF_INET   = "AF_INET"
  show AF_INET6  = "AF_INET6"

instance ToCode SocketFamily where
  toCode AF_UNSPEC = 0
  toCode AF_INET   = 2
  toCode AF_INET6  = 10

getSocketFamily : Int -> Maybe SocketFamily
getSocketFamily i = Prelude.List.lookup i [(0, AF_UNSPEC), (2, AF_INET), (10, AF_INET6)]

------------------------------------------------------------------------

||| Socket Types.
data SocketType =
  ||| Not a socket, used in certain operations
  NotASocket |
  ||| TCP
  Stream |
  ||| UDP
  Datagram |
  ||| Raw sockets
  RawSocket

instance Show SocketType where
  show NotASocket = "NotASocket"
  show Stream     = "Stream"
  show Datagram   = "Datagram"
  show RawSocket  = "Raw"

instance ToCode SocketType where
  toCode NotASocket = 0
  toCode Stream     = 1
  toCode Datagram   = 2
  toCode RawSocket  = 3

------------------------------------------------------------------------

||| Protocol number, usually zero.
ProtocolNumber : Type
ProtocolNumber = Bits32

||| Error thrown by a socket operation.
SocketError : Type
SocketError = Bits32

||| Native C socket descriptor.
SocketDescriptor : Type
SocketDescriptor = Bits32

||| Socket port.
Port : Type
Port = Bits16

||| IPv4 address.
IPv4 : Type
IPv4 = Bits32

||| Represents a socket.
record Socket : Type where
  MkSocket : (descriptor : SocketDescriptor) ->
             (family : SocketFamily) ->
             (socketType : SocketType) ->
             (protocolNumber : ProtocolNumber) ->
             Socket

instance Show Socket where
  show sock = "Socket "
           ++ show (descriptor sock) ++ " "
           ++ show (family sock) ++ " "
           ++ show (socketType sock) ++ " "
           ++ show (protocolNumber sock)

------------------------------------------------------------------------
-- Endian Conversions

htonl : Bits32 -> Bits32
htonl x = unsafePerformIO (foreign FFI_VBA "libc.dylib/htonl" (Bits32 -> VBA Bits32) x)

htons : Bits16 -> Bits16
htons x = unsafePerformIO (foreign FFI_VBA "libc.dylib/htons" (Bits16 -> VBA Bits16) x)

ntohl : Bits32 -> Bits32
ntohl x = unsafePerformIO (foreign FFI_VBA "libc.dylib/ntohl" (Bits32 -> VBA Bits32) x)

ntohs : Bits16 -> Bits16
ntohs x = unsafePerformIO (foreign FFI_VBA "libc.dylib/ntohs" (Bits16 -> VBA Bits16) x)

------------------------------------------------------------------------
-- Error Handling

peekBits32 : Ptr -> VBA Bits32
peekBits32 ptr = foreign FFI_VBA "prim$peekBits32" (Ptr -> VBA Bits32) ptr

getLastError : VBA Bits32
getLastError = do
  ptr <- foreign FFI_VBA "libc.dylib/__error" (VBA Ptr)
  peekBits32 ptr

------------------------------------------------------------------------
-- Socket Address

record SockAddr : Type where
  MkSockAddr : (sockAddrPtr : Ptr) ->
               (sockAddrLen : Bits32) ->
               SockAddr

mkSockAddr : VBA SockAddr
mkSockAddr = do
  ptr <- foreign FFI_VBA "prim$mkSockAddr" (VBA Ptr)
  len <- foreign FFI_VBA "CLng(%0.sin_len)" (Ptr -> VBA Bits32) ptr
  return (MkSockAddr ptr len)

putAddrFamily : SocketFamily -> SockAddr -> VBA ()
putAddrFamily family (MkSockAddr ptr _) =
  foreign FFI_VBA "%0.sin_family=CByte(%1)" (Ptr -> Bits32 -> VBA ()) ptr (toCode family)
  
putAddrAddr : IPv4 -> SockAddr -> VBA ()
putAddrAddr addr (MkSockAddr ptr _) =
  foreign FFI_VBA "%0.sin_addr=%1" (Ptr -> Bits32 -> VBA ()) ptr (htonl addr)
  
putAddrPort : Port -> SockAddr -> VBA ()
putAddrPort port (MkSockAddr ptr _) =
  foreign FFI_VBA "%0.sin_port=%1" (Ptr -> Bits16 -> VBA ()) ptr (htons port)

------------------------------------------------------------------------

||| Creates a UNIX socket with the given family, socket type and protocol
||| number. Returns either a socket or an error.
socket : SocketFamily -> SocketType -> ProtocolNumber -> VBA (Either SocketError Socket)
socket sf st pn = do
  socket_res <- foreign FFI_VBA "libc.dylib/socket"
                                (Bits32 -> Bits32 -> Bits32 -> VBA Bits32)
                                (toCode sf) (toCode st) pn

  if socket_res == 0xffffffff then
    Left <$> getLastError
  else
    return $ Right (MkSocket socket_res sf st pn)

||| Close a socket.
close : Socket -> VBA ()
close sock = foreign FFI_VBA "close" (SocketDescriptor -> VBA ()) (descriptor sock)

||| Binds a socket to the given socket address and port.
||| Returns Nothing on success, Just an error code otherwise.
bind : Socket -> Port -> VBA (Maybe SocketError)
bind sock port = do
  addr <- mkSockAddr
  putAddrFamily (family sock) addr
  putAddrAddr (htonl 0) addr
  putAddrPort (htons port) addr

  bind_res <- foreign FFI_VBA "libc.dylib/bind" 
                  (SocketDescriptor -> Ptr -> Bits32 -> VBA Bits32)
                  (descriptor sock) (sockAddrPtr addr) (sockAddrLen addr)

  if bind_res == 0xffffffff then
    Just <$> getLastError
  else
    return Nothing

--Private Declare Function listen Lib "libc.dylib" (ByVal socket As Long, ByVal backlog As Long) As Long
--Private Declare Function accept Lib "libc.dylib" (ByVal socket As Long, ByRef address As sockaddr_in, ByRef address_len As Long) As Long
--

