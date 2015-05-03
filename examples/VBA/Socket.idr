module VBA.Socket

import VBA.Base
import VBA.Except
import VBA.Memory
import Util

import Data.Bits

------------------------------------------------------------------------

%access public

------------------------------------------------------------------------

class ToCode a where
  toCode : a -> Int

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

fromCode : Int -> Maybe SocketFamily
fromCode 0  = Just AF_UNSPEC
fromCode 2  = Just AF_INET
fromCode 10 = Just AF_INET6
fromCode _  = Nothing

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
ErrorCode : Type
ErrorCode = Bits32

||| Native C socket descriptor.
SocketDescriptor : Type
SocketDescriptor = Bits32

||| Socket port.
Port : Type
Port = Bits16

||| IPv4 address.
IPv4 : Type
IPv4 = Bits32

||| Maximum length for the queue of pending connections.
Backlog : Type
Backlog = Bits32

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
-- Conversions

htonl : Bits32 -> Bits32
htonl x = unsafePerformIO (foreign FFI_VBA "libc.dylib/htonl" (Bits32 -> VBA Bits32) x)

htons : Bits16 -> Bits16
htons x = unsafePerformIO (foreign FFI_VBA "libc.dylib/htons" (Bits16 -> VBA Bits16) x)

ntohl : Bits32 -> Bits32
ntohl x = unsafePerformIO (foreign FFI_VBA "libc.dylib/ntohl" (Bits32 -> VBA Bits32) x)

ntohs : Bits16 -> Bits16
ntohs x = unsafePerformIO (foreign FFI_VBA "libc.dylib/ntohs" (Bits16 -> VBA Bits16) x)

inet_ntoa : IPv4 -> String
inet_ntoa addr = unsafePerformIO $ do
  ptr <- foreign FFI_VBA "libc.dylib/inet_ntoa" (Bits32 -> VBA Ptr) addr
  peekCString ptr

------------------------------------------------------------------------
-- Error Handling

getLastError : VBA ErrorCode
getLastError = do
  ptr <- foreign FFI_VBA "libc.dylib/__error" (VBA Ptr)
  peekBits32 ptr

------------------------------------------------------------------------
-- Descriptor Manipulation

private
fcntl : SocketDescriptor -> Bits32 -> Bits32 -> VBA Bits32
fcntl fd cmd arg = foreign FFI_VBA "libc.dylib/fcntl"
                           (SocketDescriptor -> Bits32 -> Bits32 -> VBA Bits32)
                           fd cmd arg

private
F_GETFL : Bits32
F_GETFL = 3

private
F_SETFL : Bits32
F_SETFL = 4

private
O_NONBLOCK : Bits32
O_NONBLOCK = 4

setNonBlocking : Socket -> VBAExcept ErrorCode ()
setNonBlocking sock = vbaM $ do
  let fd = descriptor sock
  flags  <- fcntl fd F_GETFL 0
  result <- fcntl fd F_SETFL (flags `prim__orB32` O_NONBLOCK)
  if result == 0xffffffff
     then Left <$> getLastError
     else return (Right ())

------------------------------------------------------------------------
-- Socket Address

record SocketAddress : Type where
  MkSockAddr : (family : SocketFamily) ->
               (address : IPv4) ->
               (port : Port) ->
               SocketAddress

instance Show SocketAddress where
  show (MkSockAddr AF_INET addr port) = inet_ntoa addr ++ ":" ++ show port
  show (MkSockAddr family  _    _)    = "SocketAddress(" ++ show family ++ ")"

||| The length of the C struct sockaddr_in (in bytes)
|||
||| sin_len    : Bits8
||| sin_family : Bits8
||| sin_port   : Bits16
||| sin_addr   : Bits32
||| sin_zero   : Vect 8 Bits8
||| 1 + 1 + 2 + 4 + 8 = 16 bytes
sockAddrLen : Nat
sockAddrLen = 16

allocSockAddr : VBA Ptr
allocSockAddr = do
  ptr <- calloc sockAddrLen 1
  pokeBits8 ptr (cast sockAddrLen)
  return ptr

pokeSockAddr : Ptr -> SocketAddress -> VBA ()
pokeSockAddr ptr (MkSockAddr family address port) = do
  pokeBits8  (ptr `plusPtr` 1) (cast (toCode family))
  pokeBits16 (ptr `plusPtr` 2) (htons port)
  pokeBits32 (ptr `plusPtr` 4) (htonl address)

peekSockAddr : Ptr -> VBA (Maybe SocketAddress)
peekSockAddr ptr = do
  family  <- peekBits8  (ptr `plusPtr` 1)
  port    <- peekBits16 (ptr `plusPtr` 2)
  address <- peekBits32 (ptr `plusPtr` 4)
  case fromCode (cast family) of
       Nothing => return Nothing
       Just fm => return $ Just (MkSockAddr fm (ntohl address) (ntohs port))

------------------------------------------------------------------------
-- Creating

||| Creates a UNIX socket with the given family, socket type and protocol
||| number. Returns either a socket or an error.
socket : SocketFamily -> SocketType -> ProtocolNumber -> VBAExcept ErrorCode Socket
socket family type proto = vbaM $ do
  result <- foreign FFI_VBA "libc.dylib/socket"
                            (Bits32 -> Bits32 -> Bits32 -> VBA Bits32)
                            (cast (toCode family)) (cast (toCode type)) proto

  if result == 0xffffffff
     then Left <$> getLastError
     else return $ Right (MkSocket result family type proto)

private
closeDescriptor : SocketDescriptor -> VBA ()
closeDescriptor sd = foreign FFI_VBA "libc.dylib/close" (SocketDescriptor -> VBA ()) sd

||| Close a socket.
close : Socket -> VBAExcept ErrorCode ()
close sock = ve_lift (closeDescriptor (descriptor sock))

------------------------------------------------------------------------
-- Binding

||| Binds a socket to the given socket address and port.
||| Returns Nothing on success, Just an error code otherwise.
bind : Socket -> Port -> VBAExcept ErrorCode ()
bind sock port = vbaM $ do
  addrPtr <- allocSockAddr
  pokeSockAddr addrPtr (MkSockAddr (family sock) 0 port)

  result <- foreign FFI_VBA "libc.dylib/bind"
                    (Bits32 -> Ptr -> Bits32 -> VBA Bits32)
                    (descriptor sock) addrPtr (cast sockAddrLen)

  free addrPtr

  if result == 0xffffffff
     then Left <$> getLastError
     else return (Right ())

------------------------------------------------------------------------
-- Listening/Accepting

||| Listen for connections on a socket.
listen : Socket -> Backlog -> VBAExcept ErrorCode ()
listen sock backlog = vbaM $ do
  result <- foreign FFI_VBA "libc.dylib/listen"
                    (Bits32 -> Bits32 -> VBA Bits32)
                    (descriptor sock) backlog

  if result == 0xffffffff
     then Left <$> getLastError
     else return (Right ())

||| Accept a connection on a socket.
accept : Socket -> VBAExcept ErrorCode (Maybe (Socket, SocketAddress))
accept sock = vbaM $ do
  addrPtr <- allocSockAddr

  result <- foreign FFI_VBA "libc.dylib/accept"
                    (Bits32 -> Ptr -> Bits32 -> VBA Bits32)
                    (descriptor sock) addrPtr (cast sockAddrLen)

  maddr <- peekSockAddr addrPtr
  free addrPtr

  if result == 0xffffffff
     then Left <$> getLastError
     else case maddr of
       Nothing => do
         closeDescriptor result
         return (Right Nothing)
       Just addr => do
         let csock = MkSocket (result)
                              (family addr)
                              (socketType sock)
                              (protocolNumber sock)
         return (Right (Just (csock, addr)))
