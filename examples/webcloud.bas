' Foreign Functions
Private Declare PtrSafe Function FFI296 Lib "ws2_32" Alias "accept" (ByVal arg1 As LongLong, ByVal arg2 As LongPtr, ByVal arg3 As Long) As LongLong
Private Declare PtrSafe Function FFI297 Lib "ws2_32" Alias "bind" (ByVal arg1 As LongLong, ByVal arg2 As LongPtr, ByVal arg3 As Long) As Long
Private Declare PtrSafe Function FFI298 Lib "ws2_32" Alias "htonl" (ByVal arg1 As Long) As Long
Private Declare PtrSafe Function FFI299 Lib "ws2_32" Alias "htons" (ByVal arg1 As Integer) As Integer
Private Declare PtrSafe Function FFI30 Lib "ws2_32" Alias "WSAGetLastError" () As Long
Private Declare PtrSafe Function FFI300 Lib "ws2_32" Alias "inet_ntoa" (ByVal arg1 As Long) As LongPtr
Private Declare PtrSafe Function FFI303 Lib "ws2_32" Alias "listen" (ByVal arg1 As LongLong, ByVal arg2 As Long) As Long
Private Declare PtrSafe Function FFI305 Lib "ws2_32" Alias "ntohl" (ByVal arg1 As Long) As Long
Private Declare PtrSafe Function FFI306 Lib "ws2_32" Alias "ntohs" (ByVal arg1 As Integer) As Integer
Private Declare PtrSafe Function FFI309 Lib "ws2_32" Alias "socket" (ByVal arg1 As Long, ByVal arg2 As Long, ByVal arg3 As Long) As LongLong
Private Declare PtrSafe Function FFI41 Lib "ws2_32" Alias "ioctlsocket" (ByVal arg1 As LongLong, ByVal arg2 As Long, ByVal arg3 As LongPtr) As Long
Private Declare PtrSafe Sub FFI22 Lib "ws2_32" Alias "close" (ByVal arg1 As LongLong)

' Registers
Private R0 as Variant
Private R1 as Variant
Private R2 as Variant
Private R3 as Variant
Private R4 as Variant
Private R5 as Variant
Private R6 as Variant
Private R7 as Variant
Private R8 as Variant
Private R9 as Variant

' Handle to the current process heap
Private Idris_ProcessHeap As LongPtr

' Bit patterns for shift left operations
Private Idris_OnBits8(0 To 7) As Byte
Private Idris_OnBits16(0 To 15) As Integer
Private Idris_OnBits32(0 To 31) As Long
Private Idris_OnBits64(0 To 63) As LongLong

' Runtime Foreign Functions
Private Declare PtrSafe Function Idris_GetProcessHeap Lib "kernel32" Alias "GetProcessHeap" () As LongPtr
Private Declare PtrSafe Function Idris_HeapAlloc Lib "kernel32" Alias "HeapAlloc" (ByVal hHeap As LongPtr, ByVal dwFlags As Long, ByVal dwBytes As LongPtr) As LongPtr
Private Declare PtrSafe Function Idris_HeapFree Lib "kernel32" Alias "HeapFree" (ByVal hHeap As LongPtr, ByVal dwFlags As Long, ByVal lpMem As LongPtr) As Long
Private Declare PtrSafe Sub Idris_CopyMemory Lib "kernel32" Alias "RtlCopyMemory" (ByVal dst As LongPtr, ByVal src As LongPtr, ByVal n As LongPtr)
Private Declare PtrSafe Function Idris_StrLen Lib "kernel32" Alias "lstrlenA" (ByVal ptr As LongPtr) As Long

Private Sub Idris_InitRuntime()
    Idris_ProcessHeap = Idris_GetProcessHeap
    Idris_MakeOnBits8
    Idris_MakeOnBits16
    Idris_MakeOnBits32
    Idris_MakeOnBits64
End Sub

Private Sub Idris_MakeOnBits8()
    Dim j As Integer
    Dim v As Integer
    For j = 0 To 6
        v = v + (2 ^ j)
        Idris_OnBits8(j) = v
    Next j
    Idris_OnBits8(j) = v + &H80
End Sub

Private Sub Idris_MakeOnBits16()
    Dim j As Integer
    Dim v As Integer
    For j = 0 To 14
        v = v + (2 ^ j)
        Idris_OnBits16(j) = v
    Next j
    Idris_OnBits16(j) = v + &H8000
End Sub

Private Sub Idris_MakeOnBits32()
    Dim j As Integer
    Dim v As Long
    For j = 0 To 30
        v = v + (2 ^ j)
        Idris_OnBits32(j) = v
    Next j
    Idris_OnBits32(j) = v + &H80000000&
End Sub

Private Sub Idris_MakeOnBits64()
    Dim j As Integer
    Dim v As LongLong
    Dim p As LongLong
    For j = 0 To 62
        p = 2 ^ j
        v = v + p
        Idris_OnBits64(j) = v
    Next j
    Idris_OnBits64(j) = v + &H8000000000000000^
End Sub

Private Function Idris_Alloc(ByVal nbytes As LongPtr) As LongPtr
    Idris_Alloc = Idris_HeapAlloc(Idris_ProcessHeap, 8, nbytes)
End Function

Private Function Idris_Free(ByVal ptr As LongPtr) As Boolean
    Idris_Free = Idris_HeapFree(Idris_ProcessHeap, 0, ptr) <> 0
End Function

Private Function Idris_Shl8(ByVal value As Byte, ByVal shift As Integer) As Byte
    If (value And (2 ^ (7 - shift))) Then GoTo Overflow
    Idris_Shl8 = ((value And Idris_OnBits8(7 - shift)) * (2 ^ shift))
    Exit Function
Overflow:
    Idris_Shl8 = ((value And Idris_OnBits8(7 - (shift + 1))) * (2 ^ (shift))) Or &H80
End Function

Private Function Idris_Shl16(ByVal value As Integer, ByVal shift As Integer) As Integer
    If (value And (2 ^ (15 - shift))) Then GoTo Overflow
    Idris_Shl16 = ((value And Idris_OnBits16(15 - shift)) * (2 ^ shift))
    Exit Function
Overflow:
    Idris_Shl16 = ((value And Idris_OnBits16(15 - (shift + 1))) * (2 ^ (shift))) Or &H8000
End Function

Private Function Idris_Shl32(ByVal value As Long, ByVal shift As Integer) As Long
    If (value And (2 ^ (31 - shift))) Then GoTo Overflow
    Idris_Shl32 = ((value And Idris_OnBits32(31 - shift)) * (2 ^ shift))
    Exit Function
Overflow:
    Idris_Shl32 = ((value And Idris_OnBits32(31 - (shift + 1))) * (2 ^ (shift))) Or &H80000000
End Function

Private Function Idris_Shl64(ByVal value As LongLong, ByVal shift As Integer) As LongLong
    If (value And (2 ^ (63 - shift))) Then GoTo Overflow
    Idris_Shl64 = ((value And Idris_OnBits64(63 - shift)) * (2 ^ shift))
    Exit Function
Overflow:
    Idris_Shl64 = ((value And Idris_OnBits64(31 - (shift + 1))) * (2 ^ (shift))) Or &H8000000000000000^
End Function

Private Function Idris_LShr8(ByVal value As Byte, ByVal shift As Integer) As Byte
    Dim hi As Byte
    If (value And &H80) Then hi = &H40
    Idris_LShr8 = (value And &H7E) \ (2 ^ shift)
    Idris_LShr8 = (Idris_LShr8 Or (hi \ (2 ^ (shift - 1))))
End Function

Private Function Idris_LShr16(ByVal value As Integer, ByVal shift As Integer) As Integer
    Dim hi As Integer
    If (value And &H8000) Then hi = &H4000
    Idris_LShr16 = (value And &H7FFE) \ (2 ^ shift)
    Idris_LShr16 = (Idris_LShr16 Or (hi \ (2 ^ (shift - 1))))
End Function

Private Function Idris_LShr32(ByVal value As Long, ByVal shift As Integer) As Long
    Dim hi As Long
    If (value And &H80000000) Then hi = &H40000000
    Idris_LShr32 = (value And &H7FFFFFFE) \ (2 ^ shift)
    Idris_LShr32 = (Idris_LShr32 Or (hi \ (2 ^ (shift - 1))))
End Function

Private Function Idris_LShr64(ByVal value As LongLong, ByVal shift As Integer) As LongLong
    Dim hi As LongLong
    If (value And &H8000000000000000^) Then hi = &H4000000000000000^
    Idris_LShr64 = (value And &H7FFFFFFFFFFFFFFE^) \ (2 ^ shift)
    Idris_LShr64 = (Idris_LShr64 Or (hi \ (2 ^ (shift - 1))))
End Function

Private Function Idris_UTrunc_32_16(ByVal value As Long) As Integer
    Dim masked As Long
    masked = value And &HFFFF&
    If masked < &H8000& Then
        Idris_UTrunc_32_16 = CInt(masked)
    Else
        Idris_UTrunc_32_16 = CInt(masked - &H10000)
    End If
End Function

Private Function Idris_UTrunc_64_16(ByVal value As LongLong) As Integer
    Dim masked As LongLong
    masked = value And &HFFFF^
    If masked < &H8000^ Then
        Idris_UTrunc_64_16 = CInt(masked)
    Else
        Idris_UTrunc_64_16 = CInt(masked - &H10000^)
    End If
End Function

Private Function Idris_UTrunc_64_32(ByVal value As LongLong) As Long
    Dim masked As LongLong
    masked = value And &HFFFFFFFF^
    If masked < &H80000000^ Then
        Idris_UTrunc_64_32 = CLng(masked)
    Else
        Idris_UTrunc_64_32 = CLng(masked - &H100000000^)
    End If
End Function

Private Function Idris_UGt16(ByVal x As Integer, ByVal y As Integer) As Boolean
    Idris_UGt16 = (x > y) Xor (x < 0) Xor (y < 0)
End Function

Private Function Idris_UGe16(ByVal x As Integer, ByVal y As Integer) As Boolean
    Idris_UGe16 = (x >= y) Xor (x < 0) Xor (y < 0)
End Function

Private Function Idris_ULt16(ByVal x As Integer, ByVal y As Integer) As Boolean
    Idris_ULt16 = (x < y) Xor (x < 0) Xor (y < 0)
End Function

Private Function Idris_ULe16(ByVal x As Integer, ByVal y As Integer) As Boolean
    Idris_ULe16 = (x <= y) Xor (x < 0) Xor (y < 0)
End Function

Private Function Idris_UGt32(ByVal x As Long, ByVal y As Long) As Boolean
    Idris_UGt32 = (x > y) Xor (x < 0) Xor (y < 0)
End Function

Private Function Idris_UGe32(ByVal x As Long, ByVal y As Long) As Boolean
    Idris_UGe32 = (x >= y) Xor (x < 0) Xor (y < 0)
End Function

Private Function Idris_ULt32(ByVal x As Long, ByVal y As Long) As Boolean
    Idris_ULt32 = (x < y) Xor (x < 0) Xor (y < 0)
End Function

Private Function Idris_ULe32(ByVal x As Long, ByVal y As Long) As Boolean
    Idris_ULe32 = (x <= y) Xor (x < 0) Xor (y < 0)
End Function

Private Function Idris_UGt64(ByVal x As LongLong, ByVal y As LongLong) As Boolean
    Idris_UGt64 = (x > y) Xor (x < 0) Xor (y < 0)
End Function

Private Function Idris_UGe64(ByVal x As LongLong, ByVal y As LongLong) As Boolean
    Idris_UGe64 = (x >= y) Xor (x < 0) Xor (y < 0)
End Function

Private Function Idris_ULt64(ByVal x As LongLong, ByVal y As LongLong) As Boolean
    Idris_ULt64 = (x < y) Xor (x < 0) Xor (y < 0)
End Function

Private Function Idris_ULe64(ByVal x As LongLong, ByVal y As LongLong) As Boolean
    Idris_ULe64 = (x <= y) Xor (x < 0) Xor (y < 0)
End Function

Private Function Idris_PeekBits8(ByVal ptr As LongPtr) As LongPtr
    Dim x As Byte
    Call Idris_CopyMemory(VarPtr(x), ptr, Len(x))
    Idris_PeekBits8 = x
End Function

Private Function Idris_PeekBits16(ByVal ptr As LongPtr) As Integer
    Dim x As Integer
    Call Idris_CopyMemory(VarPtr(x), ptr, Len(x))
    Idris_PeekBits16 = x
End Function

Private Function Idris_PeekBits32(ByVal ptr As LongPtr) As Long
    Dim x As Long
    Call Idris_CopyMemory(VarPtr(x), ptr, Len(x))
    Idris_PeekBits32 = x
End Function

Private Function Idris_PeekBits64(ByVal ptr As LongPtr) As LongLong
    Dim x As LongLong
    Call Idris_CopyMemory(VarPtr(x), ptr, Len(x))
    Idris_PeekBits64 = x
End Function

Private Function Idris_PeekCString(ByVal ptr As LongPtr) As String
    Dim slen As Long
    Dim str() As Byte
    slen = Idris_StrLen(ptr)
    ReDim str(0 To slen - 1)
    Call Idris_CopyMemory(VarPtr(str(0)), ptr, slen)
    Idris_PeekCString = StrConv(str, vbUnicode)
End Function

Private Sub Idris_PokeBits8(ByVal ptr As LongPtr, ByVal x As Byte)
    Call Idris_CopyMemory(ptr, VarPtr(x), Len(x))
End Sub

Private Sub Idris_PokeBits16(ByVal ptr As LongPtr, ByVal x As Integer)
    Call Idris_CopyMemory(ptr, VarPtr(x), Len(x))
End Sub

Private Sub Idris_PokeBits32(ByVal ptr As LongPtr, ByVal x As Long)
    Call Idris_CopyMemory(ptr, VarPtr(x), Len(x))
End Sub

Private Sub Idris_PokeBits64(ByVal ptr As LongPtr, ByVal x As LongLong)
    Call Idris_CopyMemory(ptr, VarPtr(x), Len(x))
End Sub

Private Function Idris_PlusPtr(ByVal ptr As LongPtr, ByVal off As LongPtr) As LongPtr
    Idris_PlusPtr = ptr + off
End Function

Private Function Idris_Error(msg)
    Call Err.Raise(10101, "Idris", msg)
End Function

Private Function Idris_WriteStr(str)
    Debug.Print str
End Function

Private Function Idris_ReadStr() As String
    Idris_ReadStr = InputBox("Input:", "Idris")
End Function

Private Function Idris_Append(ByVal xs As String, ByVal ys As String) As String
    Idris_Append = xs + ys
End Function

' Idris Functions
Private Function Idris(ByVal fn As Integer)
Select Case fn

' Prelude.Bool.&&
Case 0
Idris0:
L0 = R0
L1 = R1
Select Case L0(0)
Case 0
Idris = Array(0)
Case 1
R0 = L1
GoTo Idris1
End Select

' Prelude.Basics..
Case 2
Idris2:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
R0 = L4
R1 = L5
L6 = Idris(3)
R0 = L3
R1 = L6
GoTo Idris3

' Prelude.Classes.<
Case 4
Idris4:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
L4 = L1(3)
Idris = L3
End Select

' Prelude.Classes.==
Case 5
Idris5:
L0 = R0
L1 = R1
Idris = L1

' Prelude.Classes.>
Case 6
Idris6:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
L4 = L1(3)
Idris = L4
End Select

' Prelude.Monad.>>=
Case 7
Idris7:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
Select Case L3(0)
Case 0
L4 = L3(1)
L5 = L3(2)
R0 = L5
R1 = L1
L6 = Idris(3)
R0 = L6
R1 = L2
GoTo Idris3
End Select

' @@constructor of Prelude.Monad.Monad#Applicative m
Case 8
Idris8:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
Idris = L2
End Select

' Force
Case 9
Idris9:
L0 = R0
L1 = R1
L2 = R2
R0 = L2
L3 = Idris(1)
Idris = L3

' VBA.Socket.accept
Case 10
Idris10:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = Idris(11)
L5 = Array(65743,L0)
Idris = Array(65811,L1,L2,L3,L4,L5)

' VBA.Memory.alloc
Case 12
Idris12:
L0 = R0
L1 = R1
L2 = Idris(13)
R0 = L2
R1 = L0
L2 = Idris(3)
Idris = Idris_Alloc(L2)

' VBA.Socket.allocSockAddr
Case 11
Idris11:
L0 = 0
L1 = 0
L2 = 0
L3 = CLngLng(&H10^)
L3 = Array(65714,L3)
L4 = Array(65753)
Idris = Array(65811,L0,L1,L2,L3,L4)

' Prelude.Bits.b8ToString
Case 14
Idris14:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(65817)
L4 = ""
L5 = 0
L5 = Array(65666,L5)
R0 = L0
L6 = Idris(15)
L7 = 0
L8 = CByte(&Hf)
L8 = (L0 And L8)
R0 = L7
R1 = L8
L7 = Idris(16)
L8 = Array(0)
L7 = Array(1,L7,L8)
L6 = Array(1,L6,L7)
R0 = L1
R1 = L2
R2 = L3
R3 = L4
R4 = L5
R5 = L6
GoTo Idris17

' VBA.Socket.bind
Case 18
Idris18:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = Idris(11)
L6 = Array(65759,L0,L1)
Idris = Array(65811,L2,L3,L4,L5,L6)

' Prelude.Bool.boolElim
Case 19
Idris19:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
Select Case L1(0)
Case 0
R0 = L3
GoTo Idris1
Case 1
R0 = L2
GoTo Idris1
End Select

' call__IO
Case 20
Idris20:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
R0 = L2
R1 = L3
GoTo Idris3

' VBA.Socket.closeDescriptor
Case 21
Idris21:
L0 = R0
L1 = R1
Call FFI22(L0)
Idris = 0

' Prelude.Classes.compare
Case 23
Idris23:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
L4 = L1(3)
Idris = L2
End Select

' VBA.Socket.Socket.descriptor
Case 24
Idris24:
L0 = R0
Select Case L0(0)
Case 0
L1 = L0(1)
L2 = L0(2)
L3 = L0(3)
L4 = L0(4)
Idris = L1
End Select

' VBA.Socket.Socket.family
Case 25
Idris25:
L0 = R0
Select Case L0(0)
Case 0
L1 = L0(1)
L2 = L0(2)
L3 = L0(3)
L4 = L0(4)
Idris = L2
End Select

' VBA.Socket.SocketAddress.family
Case 26
Idris26:
L0 = R0
Select Case L0(0)
Case 0
L1 = L0(1)
L2 = L0(2)
L3 = L0(3)
Idris = L1
End Select

' Prelude.List.foldrImpl
Case 17
Idris17:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
Select Case L5(0)
Case 1
L6 = L5(1)
L7 = L5(2)
L8 = 0
L9 = 0
L10 = 0
L11 = 0
L12 = 0
R0 = L2
R1 = L6
L13 = Idris(3)
L10 = Array(65665,L10,L11,L12,L4,L13)
R0 = L8
R1 = L9
R2 = L2
R3 = L3
R4 = L10
R5 = L7
GoTo Idris17
Case 0
R0 = L4
R1 = L3
GoTo Idris3
End Select

' VBA.Memory.free
Case 27
Idris27:
L0 = R0
L1 = R1
Call Idris_Free(L0)
Idris = 0

' VBA.Socket.fromCode
Case 28
Idris28:
L0 = R0
Select Case L0
Case CLng(&H0&)
L1 = Array(0)
Idris = Array(1,L1)
Case CLng(&H2&)
L1 = Array(1)
Idris = Array(1,L1)
Case CLng(&Ha&)
L1 = Array(2)
Idris = Array(1,L1)
Case Else
Idris = Array(0)
End Select

' VBA.Socket.getLastError
Case 29
Idris29:
L0 = R0
Idris = FFI30()

' VBA.Socket.htonl
Case 31
Idris31:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(65764,L0)
R0 = L1
R1 = L2
R2 = L3
GoTo Idris32

' VBA.Socket.htons
Case 33
Idris33:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(65765,L0)
R0 = L1
R1 = L2
R2 = L3
GoTo Idris32

' Prelude.Basics.id
Case 34
Idris34:
L0 = R0
L1 = R1
Idris = L1

' VBA.Socket.inet_ntoa
Case 35
Idris35:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = 0
L5 = 0
L6 = Array(65766,L0)
L7 = Array(65767)
L3 = Array(65811,L3,L4,L5,L6,L7)
R0 = L1
R1 = L2
R2 = L3
GoTo Idris32

' Prelude.Classes.intToBool
Case 36
Idris36:
L0 = R0
Select Case L0
Case CLng(&H0&)
Idris = Array(0)
Case Else
Idris = Array(1)
End Select

' io_bind
Case 37
Idris37:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
R0 = L0
R1 = L1
R2 = L2
R3 = L3
R4 = L4
R5 = L5
L6 = Idris(38)
R0 = L3
R1 = L5
L7 = Idris(3)
R0 = L6
R1 = L7
GoTo Idris3

' io_return
Case 39
Idris39:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
Idris = L2

' VBA.Socket.ioctlsocket
Case 40
Idris40:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
Idris = FFI41(L0,L1,L2)

' Prelude.Char.isDigit
Case 42
Idris42:
L0 = R0
L1 = CInt(&H30)
R0 = L0
R1 = L1
L1 = Idris(43)
Select Case L1(0)
Case 0
Idris = Array(0)
Case 1
R0 = L0
GoTo Idris44
End Select

' VBA.Socket.listen
Case 45
Idris45:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = Array(65768,L0,L1)
L6 = Array(65770)
Idris = Array(65811,L2,L3,L4,L5,L6)

' Main.main
Case 46
Idris46:
L0 = CInt(&H1234)
L1 = CLng(&H1234abcd&)
L2 = 0
L3 = 0
L4 = 0
L5 = 0
L6 = "h16 = "
L7 = CInt(&H8)
L7 = Idris_LShr16(L0, L7)
L7 = CByte(L7 And &HFF)
R0 = L7
L7 = Idris(14)
L8 = CByte(L0 And &HFF)
R0 = L8
L8 = Idris(14)
L7 = Idris_Append(L7, L8)
L6 = Idris_Append(L6, L7)
L7 = ChrW(10)
L6 = Idris_Append(L6, L7)
R0 = L5
R1 = L6
L5 = Idris(47)
L6 = Array(65657,L1,L0)
Idris = Array(65811,L2,L3,L4,L5,L6)

' mkForeignPrim
Case 48
Idris48:
Idris = 0

' Prelude.Bool.not
Case 49
Idris49:
L0 = R0
Select Case L0(0)
Case 0
Idris = Array(1)
Case 1
Idris = Array(0)
End Select

' VBA.Socket.ntohl
Case 50
Idris50:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(65771,L0)
R0 = L1
R1 = L2
R2 = L3
GoTo Idris32

' VBA.Socket.ntohs
Case 51
Idris51:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(65772,L0)
R0 = L1
R1 = L2
R2 = L3
GoTo Idris32

' VBA.Memory.peekBits16
Case 52
Idris52:
L0 = R0
L1 = R1
Idris = Idris_PeekBits16(L0)

' VBA.Memory.peekBits32
Case 53
Idris53:
L0 = R0
L1 = R1
Idris = Idris_PeekBits32(L0)

' VBA.Memory.peekBits8
Case 54
Idris54:
L0 = R0
L1 = R1
Idris = Idris_PeekBits8(L0)

' VBA.Memory.peekCString
Case 55
Idris55:
L0 = R0
L1 = R1
Idris = Idris_PeekCString(L0)

' VBA.Socket.peekSockAddr
Case 56
Idris56:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = CLngLng(&H1^)
R0 = L0
R1 = L4
L4 = Idris(57)
L4 = Array(65718,L4)
L5 = Array(65791,L0)
Idris = Array(65811,L1,L2,L3,L4,L5)

' VBA.Memory.plusPtr
Case 57
Idris57:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = Array(65723,L0,L1)
R0 = L2
R1 = L3
R2 = L4
GoTo Idris32

' VBA.Memory.pokeBits16
Case 58
Idris58:
L0 = R0
L1 = R1
L2 = R2
Call Idris_PokeBits16(L0,L1)
Idris = 0

' VBA.Memory.pokeBits32
Case 59
Idris59:
L0 = R0
L1 = R1
L2 = R2
Call Idris_PokeBits32(L0,L1)
Idris = 0

' VBA.Memory.pokeBits8
Case 60
Idris60:
L0 = R0
L1 = R1
L2 = R2
Call Idris_PokeBits8(L0,L1)
Idris = 0

' VBA.Socket.pokeSockAddr
Case 61
Idris61:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
L4 = L1(3)
L5 = 0
L6 = 0
L7 = 0
L8 = CLngLng(&H1^)
R0 = L0
R1 = L8
L8 = Idris(57)
R0 = L2
L9 = Idris(62)
L9 = CByte(L9 And &HFF)
L8 = Array(65722,L8,L9)
L9 = Array(65801,L0,L4,L3)
Idris = Array(65811,L5,L6,L7,L8,L9)
End Select

' prim__addChar
Case 63
Idris63:
L0 = R0
L1 = R1
Idris = (L0 + L1)

' prim__addInt
Case 64
Idris64:
L0 = R0
L1 = R1
Idris = (L0 + L1)

' prim__andB32
Case 65
Idris65:
L0 = R0
L1 = R1
Idris = (L0 And L1)

' prim__andB64
Case 66
Idris66:
L0 = R0
L1 = R1
Idris = (L0 And L1)

' prim__andB8
Case 67
Idris67:
L0 = R0
L1 = R1
Idris = (L0 And L1)

' prim__charToInt
Case 68
Idris68:
L0 = R0
Idris = CLng(L0)

' prim__concat
Case 69
Idris69:
L0 = R0
L1 = R1
Idris = Idris_Append(L0, L1)

' prim__eqB32
Case 70
Idris70:
L0 = R0
L1 = R1
Idris = (L0 = L1)

' prim__eqB64
Case 71
Idris71:
L0 = R0
L1 = R1
Idris = (L0 = L1)

' prim__eqChar
Case 72
Idris72:
L0 = R0
L1 = R1
Idris = (L0 = L1)

' prim__eqInt
Case 73
Idris73:
L0 = R0
L1 = R1
Idris = (L0 = L1)

' prim__eqString
Case 74
Idris74:
L0 = R0
L1 = R1
Idris = (L0 = L1)

' prim__intToChar
Case 75
Idris75:
L0 = R0
Idris = CInt(L0)

' prim__lshrB16
Case 76
Idris76:
L0 = R0
L1 = R1
Idris = Idris_LShr16(L0, L1)

' prim__lshrB32
Case 77
Idris77:
L0 = R0
L1 = R1
Idris = Idris_LShr32(L0, L1)

' prim__lshrB64
Case 78
Idris78:
L0 = R0
L1 = R1
Idris = Idris_LShr64(L0, L1)

' prim__lshrB8
Case 79
Idris79:
L0 = R0
L1 = R1
Idris = Idris_LShr8(L0, L1)

' prim__sextInt_BigInt
Case 80
Idris80:
L0 = R0
Idris = CLngLng(L0)

' prim__sltChar
Case 81
Idris81:
L0 = R0
L1 = R1
Idris = (L0 < L1)

' prim__sltInt
Case 82
Idris82:
L0 = R0
L1 = R1
Idris = (L0 < L1)

' prim__strCons
Case 83
Idris83:
L0 = R0
L1 = R1
Idris = Idris_Append(Chr(L0), L1)

' prim__strHead
Case 84
Idris84:
L0 = R0
Idris = Asc(Left(L0, 1))

' prim__strTail
Case 85
Idris85:
L0 = R0
Idris = Mid(L0, 2)

' prim__subInt
Case 86
Idris86:
L0 = R0
L1 = R1
Idris = (L0 - L1)

' prim__toStrInt
Case 87
Idris87:
L0 = R0
Idris = CStr(L0)

' prim__truncB16_B8
Case 88
Idris88:
L0 = R0
Idris = CByte(L0 And &HFF)

' prim__truncB32_B16
Case 89
Idris89:
L0 = R0
Idris = Idris_UTrunc_32_16(L0)

' prim__truncB64_B32
Case 90
Idris90:
L0 = R0
Idris = Idris_UTrunc_64_32(L0)

' prim__truncInt_B32
Case 91
Idris91:
L0 = R0
Idris = L0

' prim__truncInt_B64
Case 92
Idris92:
L0 = R0
Idris = CLngLng(L0)

' prim__truncInt_B8
Case 93
Idris93:
L0 = R0
Idris = CByte(L0 And &HFF)

' prim__writeString
Case 94
Idris94:
L0 = R0
L1 = R1
Idris = Idris_WriteStr(L1)

' prim__zextB8_Int
Case 95
Idris95:
L0 = R0
Idris = CLng(L0 And &HFF^)

' prim_io_bind
Case 96
Idris96:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
R0 = L3
R1 = L2
GoTo Idris3

' Main.printErrorCode
Case 97
Idris97:
L0 = R0
L1 = 0
L2 = "Socket error: "
R0 = L0
L3 = Idris(98)
L4 = CInt(&H8)
L3 = Idris_LShr16(L3, L4)
L3 = CByte(L3 And &HFF)
R0 = L3
L3 = Idris(14)
R0 = L0
L4 = Idris(98)
L4 = CByte(L4 And &HFF)
R0 = L4
L4 = Idris(14)
L3 = Idris_Append(L3, L4)
L4 = Idris_UTrunc_32_16(L0)
L5 = CInt(&H8)
L4 = Idris_LShr16(L4, L5)
L4 = CByte(L4 And &HFF)
R0 = L4
L4 = Idris(14)
L5 = Idris_UTrunc_32_16(L0)
L5 = CByte(L5 And &HFF)
R0 = L5
L5 = Idris(14)
L4 = Idris_Append(L4, L5)
L3 = Idris_Append(L3, L4)
L2 = Idris_Append(L2, L3)
L3 = ChrW(10)
L2 = Idris_Append(L2, L3)
R0 = L1
R1 = L2
GoTo Idris47

' Prelude.protectEsc
Case 99
Idris99:
L0 = R0
L1 = R1
L2 = R2
R0 = L2
L3 = Idris(100)
Select Case L3(0)
Case 1
L4 = L3(1)
L5 = L3(2)
R0 = L0
R1 = L4
L3 = Idris(3)
Case 0
L3 = Array(0)
End Select
Select Case L3(0)
Case 0
L3 = ""
Case 1
L3 = "\&"
End Select
L3 = Idris_Append(L3, L2)
Idris = Idris_Append(L1, L3)

' VBA.Socket.Socket.protocolNumber
Case 101
Idris101:
L0 = R0
Select Case L0(0)
Case 0
L1 = L0(1)
L2 = L0(2)
L3 = L0(3)
L4 = L0(4)
Idris = L4
End Select

' Prelude.Applicative.pure
Case 102
Idris102:
L0 = R0
L1 = R1
L2 = R2
R0 = L2
R1 = L1
GoTo Idris3

' Prelude.putStr
Case 47
Idris47:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = Array(65697,L1)
L6 = Array(65698)
Idris = Array(65811,L2,L3,L4,L5,L6)

' really_believe_me
Case 103
Idris103:
L0 = R0
L1 = R1
L2 = R2
Idris = L2

' run__IO
Case 104
Idris104:
L0 = R0
L1 = R1
L2 = 0
R0 = L1
R1 = L2
GoTo Idris3

' VBA.Socket.setNonBlocking
Case 105
Idris105:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = CLngLng(&H4^)
L4 = Array(65714,L4)
L5 = Array(65807,L0)
Idris = Array(65811,L1,L2,L3,L4,L5)

' Prelude.showLitChar
Case 106
Idris106:
L0 = R0
Select Case L0
Case CInt(&H7)
Idris = Array(65699)
Case CInt(&H8)
Idris = Array(65701)
Case CInt(&H9)
Idris = Array(65702)
Case CInt(&Ha)
Idris = Array(65703)
Case CInt(&Hb)
Idris = Array(65704)
Case CInt(&Hc)
Idris = Array(65705)
Case CInt(&Hd)
Idris = Array(65706)
Case CInt(&He)
L1 = Array(65707)
L2 = "\SO"
Idris = Array(65695,L1,L2)
Case CInt(&H5c)
Idris = Array(65708)
Case CInt(&H7f)
Idris = Array(65709)
Case Else
L1 = 0
L2 = CLng(L0)
L2 = CLngLng(L2)
L3 = 0
R0 = L3
L3 = Idris(107)
R0 = L1
R1 = L2
R2 = L3
L1 = Idris(108)
Select Case L1(0)
Case 1
L2 = L1(1)
L3 = 0
L4 = 0
L5 = 0
L6 = CInt(&H5c)
L6 = Array(65813,L6)
L7 = Array(65700,L2)
Idris = Array(65665,L3,L4,L5,L6,L7)
Case 0
L2 = 0
L3 = Idris(109)
R0 = L2
R1 = L3
L2 = Idris(23)
R0 = L2
R1 = L0
L2 = Idris(3)
L3 = CInt(&H7f)
R0 = L2
R1 = L3
L2 = Idris(3)
Select Case L2(0)
Case 2
L2 = Array(1)
Case Else
L2 = Array(0)
End Select
Select Case L2(0)
Case 0
Idris = Array(65813,L0)
Case 1
L3 = 0
L4 = 0
L5 = 0
L6 = CInt(&H5c)
L6 = Array(65813,L6)
L7 = Array(65673)
L8 = CLng(L0)
L8 = CStr(L8)
L7 = Array(65695,L7,L8)
Idris = Array(65665,L3,L4,L5,L6,L7)
End Select
End Select
End Select

' Prelude.showLitString
Case 110
Idris110:
L0 = R0
Select Case L0(0)
Case 1
L1 = L0(1)
L2 = L0(2)
Select Case L1
Case CInt(&H22)
L3 = 0
L4 = 0
L5 = 0
L6 = Array(65710)
R0 = L2
L7 = Idris(110)
Idris = Array(65665,L3,L4,L5,L6,L7)
Case Else
L3 = 0
L4 = 0
L5 = 0
R0 = L1
L6 = Idris(106)
R0 = L2
L7 = Idris(110)
Idris = Array(65665,L3,L4,L5,L6,L7)
End Select
Case 0
L1 = 0
Idris = Array(65666,L1)
End Select

' VBA.Socket.socket
Case 111
Idris111:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
L6 = Array(65808,L0,L1,L2)
L7 = Array(65810,L0,L1,L2)
Idris = Array(65811,L3,L4,L5,L6,L7)

' VBA.Socket.Socket.socketType
Case 112
Idris112:
L0 = R0
Select Case L0(0)
Case 0
L1 = L0(1)
L2 = L0(2)
L3 = L0(3)
L4 = L0(4)
Idris = L3
End Select

' Prelude.Strings.strM
Case 100
Idris100:
L0 = R0
L1 = ""
L1 = (L0 = L1)
Select Case L1
Case CLng(&H0&)
L1 = Array(0)
Case Else
L1 = Array(1)
End Select
Select Case L1(0)
Case 0
L1 = Array(1)
Case 1
L1 = Array(0)
End Select
L2 = Array(1)
R0 = L1
R1 = L2
L1 = Idris(113)
Select Case L1(0)
Case 1
L2 = 0
L3 = 0
L4 = Array(0)
R0 = L2
R1 = L3
R2 = L4
GoTo Idris103
Case 0
L2 = 0
L3 = 0
L4 = Asc(Left(L0, 1))
L5 = Mid(L0, 2)
L4 = Array(1,L4,L5)
R0 = L2
R1 = L3
R2 = L4
GoTo Idris103
End Select

' Prelude.Strings.unpack
Case 114
Idris114:
L0 = R0
R0 = L0
L1 = Idris(100)
Select Case L1(0)
Case 1
L2 = L1(1)
L3 = L1(2)
R0 = L3
L4 = Idris(114)
Idris = Array(1,L2,L4)
Case 0
Idris = Array(0)
End Select

' unsafePerformIO
Case 32
Idris32:
L0 = R0
L1 = R1
L2 = R2
R0 = L0
R1 = L1
R2 = L2
L3 = Idris(115)
L4 = 0
R0 = L2
R1 = L4
L4 = Idris(3)
R0 = L3
R1 = L4
GoTo Idris3

' unsafePerformPrimIO
Case 116
Idris116:
Idris = 0

' VBA.Except.ve_handle
Case 117
Idris117:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = 0
L5 = 0
L6 = 0
L7 = Array(65711)
R0 = L4
R1 = L5
R2 = L6
R3 = L3
R4 = L2
R5 = L7
GoTo Idris118

' VBA.Except.ve_lift
Case 119
Idris119:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
L6 = Array(65712)
Idris = Array(65811,L3,L4,L5,L2,L6)

' VBA.Except.ve_run
Case 118
Idris118:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = 0
L7 = 0
L8 = 0
L9 = Array(65713,L4,L5)
Idris = Array(65811,L6,L7,L8,L3,L9)

' world
Case 120
Idris120:
L0 = R0
Idris = L0

' Prelude.Bool.||
Case 121
Idris121:
L0 = R0
L1 = R1
Select Case L0(0)
Case 0
R0 = L1
GoTo Idris1
Case 1
Idris = Array(1)
End Select

' {APPLY0}
Case 3
Idris3:
L0 = R0
L1 = R1
Select Case L0(0)
Case 65631
R0 = L1
GoTo Idris97
Case 65632
R0 = L1
GoTo Idris122
Case 65633
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris123
Case 65634
R0 = L1
GoTo Idris124
Case 65635
R0 = L1
GoTo Idris125
Case 65636
R0 = L1
GoTo Idris126
Case 65637
R0 = L1
GoTo Idris127
Case 65638
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris128
Case 65639
R0 = L1
GoTo Idris129
Case 65640
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris130
Case 65641
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris131
Case 65642
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris132
Case 65643
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris133
Case 65644
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris134
Case 65645
R0 = L1
GoTo Idris135
Case 65646
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris136
Case 65647
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris137
Case 65648
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris138
Case 65649
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris139
Case 65650
R0 = L1
GoTo Idris140
Case 65651
R0 = L1
GoTo Idris141
Case 65652
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris142
Case 65653
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris143
Case 65654
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris144
Case 65655
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris145
Case 65656
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris146
Case 65657
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris147
Case 65658
R0 = L1
GoTo Idris148
Case 65659
R0 = L1
GoTo Idris149
Case 65660
R0 = L1
GoTo Idris150
Case 65661
R0 = L1
GoTo Idris151
Case 65662
R0 = L1
GoTo Idris152
Case 65663
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris153
Case 65664
R0 = L1
GoTo Idris154
Case 65665
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
L5 = L0(4)
L6 = L0(5)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
R4 = L6
R5 = L1
GoTo Idris2
Case 65666
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris34
Case 65667
R0 = L1
GoTo Idris155
Case 65668
R0 = L1
GoTo Idris156
Case 65669
R0 = L1
GoTo Idris157
Case 65670
R0 = L1
GoTo Idris158
Case 65671
R0 = L1
GoTo Idris159
Case 65672
R0 = L1
GoTo Idris160
Case 65673
R0 = L1
GoTo Idris42
Case 65674
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris161
Case 65675
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris162
Case 65676
R0 = L1
GoTo Idris163
Case 65677
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris164
Case 65678
R0 = L1
GoTo Idris165
Case 65679
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris166
Case 65680
R0 = L1
GoTo Idris167
Case 65681
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris168
Case 65682
R0 = L1
GoTo Idris169
Case 65683
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris170
Case 65684
R0 = L1
GoTo Idris171
Case 65685
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris172
Case 65686
R0 = L1
GoTo Idris173
Case 65687
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris174
Case 65688
R0 = L1
GoTo Idris175
Case 65689
R0 = L1
GoTo Idris176
Case 65690
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris177
Case 65691
R0 = L1
GoTo Idris178
Case 65692
R0 = L1
GoTo Idris179
Case 65693
R0 = L1
GoTo Idris180
Case 65694
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris181
Case 65695
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris99
Case 65696
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris182
Case 65697
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris183
Case 65698
R0 = L1
GoTo Idris184
Case 65699
R0 = L1
GoTo Idris185
Case 65700
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris186
Case 65701
R0 = L1
GoTo Idris187
Case 65702
R0 = L1
GoTo Idris188
Case 65703
R0 = L1
GoTo Idris189
Case 65704
R0 = L1
GoTo Idris190
Case 65705
R0 = L1
GoTo Idris191
Case 65706
R0 = L1
GoTo Idris192
Case 65707
R0 = L1
GoTo Idris193
Case 65708
R0 = L1
GoTo Idris194
Case 65709
R0 = L1
GoTo Idris195
Case 65710
R0 = L1
GoTo Idris196
Case 65711
R0 = L1
GoTo Idris197
Case 65712
R0 = L1
GoTo Idris198
Case 65713
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris199
Case 65714
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris12
Case 65715
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris27
Case 65716
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris52
Case 65717
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris53
Case 65718
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris54
Case 65719
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris55
Case 65720
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris58
Case 65721
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris59
Case 65722
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris60
Case 65723
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris200
Case 65724
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris21
Case 65725
R0 = L1
GoTo Idris29
Case 65726
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris40
Case 65727
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris201
Case 65728
R0 = L1
GoTo Idris202
Case 65729
R0 = L1
GoTo Idris203
Case 65730
R0 = L1
GoTo Idris204
Case 65731
R0 = L1
GoTo Idris205
Case 65732
R0 = L1
GoTo Idris206
Case 65733
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris207
Case 65734
R0 = L1
GoTo Idris208
Case 65735
R0 = L1
GoTo Idris209
Case 65736
R0 = L1
GoTo Idris210
Case 65737
R0 = L1
GoTo Idris211
Case 65738
R0 = L1
GoTo Idris212
Case 65739
R0 = L1
GoTo Idris213
Case 65740
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris214
Case 65741
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris215
Case 65742
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris216
Case 65743
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris217
Case 65744
R0 = L1
GoTo Idris218
Case 65745
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris219
Case 65746
R0 = L1
GoTo Idris220
Case 65747
R0 = L1
GoTo Idris221
Case 65748
R0 = L1
GoTo Idris222
Case 65749
R0 = L1
GoTo Idris223
Case 65750
R0 = L1
GoTo Idris224
Case 65751
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris225
Case 65752
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris226
Case 65753
R0 = L1
GoTo Idris227
Case 65754
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris228
Case 65755
R0 = L1
GoTo Idris229
Case 65756
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris230
Case 65757
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris231
Case 65758
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris232
Case 65759
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris233
Case 65760
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris234
Case 65761
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris235
Case 65762
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris236
Case 65763
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris237
Case 65764
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris238
Case 65765
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris239
Case 65766
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris240
Case 65767
R0 = L1
GoTo Idris241
Case 65768
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris242
Case 65769
R0 = L1
GoTo Idris243
Case 65770
R0 = L1
GoTo Idris244
Case 65771
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris245
Case 65772
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris246
Case 65773
R0 = L1
GoTo Idris247
Case 65774
R0 = L1
GoTo Idris248
Case 65775
R0 = L1
GoTo Idris249
Case 65776
R0 = L1
GoTo Idris250
Case 65777
R0 = L1
GoTo Idris251
Case 65778
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris252
Case 65779
R0 = L1
GoTo Idris253
Case 65780
R0 = L1
GoTo Idris254
Case 65781
R0 = L1
GoTo Idris255
Case 65782
R0 = L1
GoTo Idris256
Case 65783
R0 = L1
GoTo Idris257
Case 65784
R0 = L1
GoTo Idris258
Case 65785
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris259
Case 65786
R0 = L1
GoTo Idris260
Case 65787
R0 = L1
GoTo Idris261
Case 65788
R0 = L1
GoTo Idris262
Case 65789
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris263
Case 65790
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris264
Case 65791
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris265
Case 65792
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris266
Case 65793
R0 = L1
GoTo Idris267
Case 65794
R0 = L1
GoTo Idris268
Case 65795
R0 = L1
GoTo Idris269
Case 65796
R0 = L1
GoTo Idris270
Case 65797
R0 = L1
GoTo Idris271
Case 65798
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris272
Case 65799
R0 = L1
GoTo Idris273
Case 65800
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris274
Case 65801
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris275
Case 65802
R0 = L1
GoTo Idris276
Case 65803
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris277
Case 65804
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris278
Case 65805
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris279
Case 65806
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris280
Case 65807
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris281
Case 65808
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris282
Case 65809
R0 = L1
GoTo Idris283
Case 65810
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris284
Case 65811
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
L5 = L0(4)
L6 = L0(5)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
R4 = L6
R5 = L1
GoTo Idris37
Case 65812
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris39
Case 65813
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris83
Case 65814
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
L5 = L0(4)
L6 = L0(5)
L7 = L0(6)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
R4 = L6
R5 = L7
R6 = L1
GoTo Idris285
Case 65815
R0 = L1
GoTo Idris286
Case 65816
Idris = Array(65674,L1)
Case 65817
Idris = Array(65813,L1)
Case Else
Idris = 0
End Select

' Prelude.Classes.{Char instance of Prelude.Classes.Ord0}
Case 162
Idris162:
L0 = R0
L1 = R1
R0 = L0
R1 = L1
GoTo Idris287

' {EVAL0}
Case 1
Idris1:
L0 = R0
Select Case L0
Case Else
Idris = L0
End Select

' Prelude.Classes.{Int instance of Prelude.Classes.Ord0}
Case 168
Idris168:
L0 = R0
L1 = R1
R0 = L0
R1 = L1
GoTo Idris288

' {Prelude.Bits.b8ToString, getDigit0}
Case 289
Idris289:
L0 = R0
L1 = CLng(&Hf&)
R0 = L0
R1 = L1
GoTo Idris290

' Prelude.Classes.{Prelude.Classes.Char instance of Prelude.Classes.Ord, method <=0}
Case 291
Idris291:
L0 = R0
L1 = R1
L2 = (L0 = L1)
Select Case L2
Case CLng(&H0&)
Idris = Array(0)
Case Else
Idris = Array(1)
End Select

' Prelude.Classes.{Prelude.Classes.Char instance of Prelude.Classes.Ord, method >=0}
Case 292
Idris292:
L0 = R0
L1 = R1
L2 = (L0 = L1)
Select Case L2
Case CLng(&H0&)
Idris = Array(0)
Case Else
Idris = Array(1)
End Select

' Prelude.Classes.{Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=0}
Case 293
Idris293:
L0 = R0
L1 = R1
L2 = (L0 = L1)
Select Case L2
Case CLng(&H0&)
Idris = Array(0)
Case Else
Idris = Array(1)
End Select

' Prelude.Classes.{Prelude.Classes.Int instance of Prelude.Classes.Ord, method >=0}
Case 294
Idris294:
L0 = R0
L1 = R1
L2 = (L0 = L1)
Select Case L2
Case CLng(&H0&)
Idris = Array(0)
Case Else
Idris = Array(1)
End Select

' Prelude.Functor.{Prelude.IO' ffi instance of Prelude.Functor.Functor, method map0}
Case 174
Idris174:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
R0 = L0
R1 = L1
L4 = Idris(3)
Idris = Array(65812,L2,L3,L4)

' Prelude.Cast.{Util.Nat, Bits32 instance of Prelude.Cast.Cast, method cast0}
Case 155
Idris155:
L0 = R0
Idris = L0

' Prelude.Cast.{Util.Nat, Bits64 instance of Prelude.Cast.Cast, method cast0}
Case 157
Idris157:
L0 = R0
Idris = CLngLng(L0)

' Prelude.Cast.{Util.Nat, Bits8 instance of Prelude.Cast.Cast, method cast0}
Case 159
Idris159:
L0 = R0
Idris = CByte(L0 And &HFF)

' Prelude.Monad.{VBA.Except.VBAExcept e instance of Prelude.Monad.Monad, method >>=0}
Case 175
Idris175:
L0 = R0
L1 = 0
L2 = 0
Idris = Array(65812,L1,L2,L0)

' VBA.Socket.{accept0}
Case 201
Idris201:
L0 = R0
L1 = R1
L2 = R2
Select Case L0(0)
Case 0
L3 = L0(1)
L4 = L0(2)
L5 = L0(3)
L6 = L0(4)
L3 = L3
End Select
L4 = Idris(295)
L5 = CLngLng(&H10^)
R0 = L4
R1 = L5
L4 = Idris(3)
Idris = FFI296(L3,L1,L4)

' VBA.Socket.{allocSockAddr0}
Case 226
Idris226:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
Idris = Array(65812,L2,L3,L0)

' VBA.Socket.{bind0}
Case 228
Idris228:
L0 = R0
L1 = R1
L2 = R2
Select Case L0(0)
Case 0
L3 = L0(1)
L4 = L0(2)
L5 = L0(3)
L6 = L0(4)
L3 = L3
End Select
L4 = Idris(295)
L5 = CLngLng(&H10^)
R0 = L4
R1 = L5
L4 = Idris(3)
Idris = FFI297(L3,L1,L4)

' VBA.Socket.{case block in accept0}
Case 234
Idris234:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
Select Case L0(0)
Case 0
L4 = L0(1)
L5 = L0(2)
L4 = L4
End Select
R0 = L2
R1 = L3
R2 = L4
L2 = Idris(102)
L3 = Array(0)
L3 = Array(1,L3)
R0 = L2
R1 = L3
GoTo Idris3

' VBA.Socket.{case block in case block in peekSockAddr0}
Case 235
Idris235:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = CLng(L0 And &HFF^)
R0 = L4
L4 = Idris(28)
Select Case L4(0)
Case 1
L5 = L4(1)
L6 = 0
L7 = 0
Select Case L1(0)
Case 0
L8 = L1(1)
L9 = L1(2)
L8 = L8
End Select
R0 = L6
R1 = L7
R2 = L8
L6 = Idris(102)
R0 = L3
L7 = Idris(50)
R0 = L2
L8 = Idris(51)
L7 = Array(0,L5,L7,L8)
L7 = Array(1,L7)
R0 = L6
R1 = L7
GoTo Idris3
Case 0
L5 = 0
L6 = 0
Select Case L1(0)
Case 0
L7 = L1(1)
L8 = L1(2)
L7 = L7
End Select
R0 = L5
R1 = L6
R2 = L7
L5 = Idris(102)
L6 = Array(0)
R0 = L5
R1 = L6
GoTo Idris3
End Select

' Main.{case block in main0}
Case 122
Idris122:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = "Client socket closed" & ChrW(10)
R0 = L3
R1 = L4
L3 = Idris(47)
R0 = L1
R1 = L2
R2 = L3
GoTo Idris119

' VBA.Socket.{case block in peekSockAddr0}
Case 236
Idris236:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = CLng(L0 And &HFF^)
R0 = L4
L4 = Idris(28)
Select Case L4(0)
Case 1
L5 = L4(1)
L6 = 0
L7 = 0
Select Case L1(0)
Case 0
L8 = L1(1)
L9 = L1(2)
L8 = L8
End Select
R0 = L6
R1 = L7
R2 = L8
L6 = Idris(102)
R0 = L3
L7 = Idris(50)
R0 = L2
L8 = Idris(51)
L7 = Array(0,L5,L7,L8)
L7 = Array(1,L7)
R0 = L6
R1 = L7
GoTo Idris3
Case 0
L5 = 0
L6 = 0
Select Case L1(0)
Case 0
L7 = L1(1)
L8 = L1(2)
L7 = L7
End Select
R0 = L5
R1 = L6
R2 = L7
L5 = Idris(102)
L6 = Array(0)
R0 = L5
R1 = L6
GoTo Idris3
End Select

' Prelude.{case block in showLitChar0}
Case 182
Idris182:
L0 = R0
L1 = R1
Idris = Idris_Append(L0, L1)

' VBA.Socket.{htonl0}
Case 238
Idris238:
L0 = R0
L1 = R1
Idris = FFI298(L0)

' VBA.Socket.{htons0}
Case 239
Idris239:
L0 = R0
L1 = R1
Idris = FFI299(L0)

' VBA.Socket.{inet_ntoa0}
Case 240
Idris240:
L0 = R0
L1 = R1
Idris = FFI300(L0)

' {io_bind0}
Case 301
Idris301:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = R6
R0 = L4
R1 = L6
GoTo Idris3

' Prelude.Char.{isDigit0}
Case 44
Idris44:
L0 = R0
L1 = CInt(&H39)
R0 = L0
R1 = L1
GoTo Idris302

' VBA.Socket.{listen0}
Case 242
Idris242:
L0 = R0
L1 = R1
L2 = R2
Select Case L0(0)
Case 0
L3 = L0(1)
L4 = L0(2)
L5 = L0(3)
L6 = L0(4)
L3 = L3
End Select
Idris = FFI303(L3,L1)

' Main.{main0}
Case 124
Idris124:
L0 = R0
L1 = 0
L2 = 0
R0 = L1
R1 = L2
R2 = L0
GoTo Idris304

' VBA.Socket.{ntohl0}
Case 245
Idris245:
L0 = R0
L1 = R1
Idris = FFI305(L0)

' VBA.Socket.{ntohs0}
Case 246
Idris246:
L0 = R0
L1 = R1
Idris = FFI306(L0)

' VBA.Socket.{peekSockAddr0}
Case 247
Idris247:
L0 = R0
L1 = 0
L2 = 0
Idris = Array(65812,L1,L2,L0)

' VBA.Memory.{plusPtr0}
Case 200
Idris200:
L0 = R0
L1 = R1
L2 = R2
L3 = Idris(295)
R0 = L3
R1 = L1
L3 = Idris(3)
Idris = Idris_PlusPtr(L0,L3)

' VBA.Socket.{pokeSockAddr0}
Case 274
Idris274:
L0 = R0
L1 = R1
L2 = R2
L3 = CLngLng(&H4^)
R0 = L0
R1 = L3
L3 = Idris(57)
R0 = L1
L4 = Idris(31)
Idris = Array(65721,L3,L4)

' Prelude.{putStr0}
Case 183
Idris183:
L0 = R0
L1 = R1
Idris = Idris_WriteStr(L0)

' {runMain0}
Case 307
Idris307:
L0 = Idris(46)
L1 = 0
R0 = L0
R1 = L1
L0 = Idris(3)
R0 = L0
GoTo Idris1

' VBA.Socket.{setNonBlocking0}
Case 276
Idris276:
L0 = R0
Idris = Array(0,L0)

' Prelude.{showLitChar0}
Case 185
Idris185:
L0 = R0
L1 = "\a"
Idris = Idris_Append(L1, L0)

' Prelude.{showLitString0}
Case 196
Idris196:
L0 = R0
L1 = "\""
Idris = Idris_Append(L1, L0)

' VBA.Socket.{socket0}
Case 282
Idris282:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
R0 = L0
L4 = Idris(62)
L4 = L4
R0 = L1
L5 = Idris(308)
L5 = L5
Idris = FFI309(L4,L5,L2)

' {unsafePerformIO0}
Case 286
Idris286:
L0 = R0
Idris = L0

' VBA.Except.{ve_handle0}
Case 197
Idris197:
L0 = R0
L1 = 0
L2 = 0
Idris = Array(65812,L1,L2,L0)

' VBA.Except.{ve_lift0}
Case 198
Idris198:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(1,L0)
Idris = Array(65812,L1,L2,L3)

' VBA.Except.{ve_run0}
Case 199
Idris199:
L0 = R0
L1 = R1
L2 = R2
Select Case L2(0)
Case 0
L3 = L2(1)
R0 = L0
R1 = L3
GoTo Idris3
Case 1
L3 = L2(1)
R0 = L1
R1 = L3
GoTo Idris3
End Select

' Prelude.Classes.{Char instance of Prelude.Classes.Ord1}
Case 163
Idris163:
L0 = R0
Idris = Array(65675,L0)

' Prelude.Classes.{Int instance of Prelude.Classes.Ord1}
Case 169
Idris169:
L0 = R0
Idris = Array(65681,L0)

' {Prelude.Bits.b8ToString, getDigit1}
Case 310
Idris310:
L0 = R0
L1 = CLng(&H9&)
R0 = L0
R1 = L1
GoTo Idris290

' Prelude.Cast.{Util.Nat, Bits32 instance of Prelude.Cast.Cast, method cast1}
Case 156
Idris156:
L0 = R0
L1 = 0
L2 = CLng(&H0&)
R0 = L1
R1 = L0
R2 = L2
GoTo Idris311

' Prelude.Cast.{Util.Nat, Bits64 instance of Prelude.Cast.Cast, method cast1}
Case 158
Idris158:
L0 = R0
L1 = 0
L2 = CLng(&H0&)
R0 = L1
R1 = L0
R2 = L2
GoTo Idris311

' Prelude.Cast.{Util.Nat, Bits8 instance of Prelude.Cast.Cast, method cast1}
Case 160
Idris160:
L0 = R0
L1 = 0
L2 = CLng(&H0&)
R0 = L1
R1 = L0
R2 = L2
GoTo Idris311

' Prelude.Monad.{VBA.Except.VBAExcept e instance of Prelude.Monad.Monad, method >>=1}
Case 176
Idris176:
L0 = R0
Idris = Array(65688)

' VBA.Socket.{accept1}
Case 212
Idris212:
L0 = R0
L1 = 0
L2 = 0
Idris = Array(65812,L1,L2,L0)

' VBA.Socket.{allocSockAddr1}
Case 227
Idris227:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = Idris(312)
L5 = CLngLng(&H10^)
R0 = L4
R1 = L5
L4 = Idris(3)
L4 = Array(65722,L0,L4)
L5 = Array(65752,L0)
Idris = Array(65811,L1,L2,L3,L4,L5)

' VBA.Socket.{bind1}
Case 229
Idris229:
L0 = R0
Idris = Array(0,L0)

' Main.{case block in main1}
Case 123
Idris123:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
R0 = L3
R1 = L4
R2 = L5
R3 = L0
L3 = Idris(7)
L4 = 0
L5 = 0
Select Case L1(0)
Case 0
L6 = L1(1)
L7 = L1(2)
L8 = L1(3)
L9 = L1(4)
L6 = L6
End Select
L6 = Array(65724,L6)
R0 = L4
R1 = L5
R2 = L6
L4 = Idris(119)
R0 = L3
R1 = L4
L3 = Idris(3)
L4 = Array(65632)
R0 = L3
R1 = L4
GoTo Idris3

' VBA.Socket.{case block in peekSockAddr1}
Case 237
Idris237:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = 0
L5 = 0
L6 = 0
R0 = L4
R1 = L5
R2 = L6
R3 = L0
L4 = Idris(7)
L5 = CLngLng(&H4^)
R0 = L1
R1 = L5
L5 = Idris(57)
L5 = Array(65717,L5)
R0 = L4
R1 = L5
L4 = Idris(3)
L5 = Array(65762,L2,L0,L3)
R0 = L4
R1 = L5
GoTo Idris3

' VBA.Socket.{inet_ntoa1}
Case 241
Idris241:
L0 = R0
Idris = Array(65719,L0)

' {io_bind1}
Case 285
Idris285:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = R6
R0 = L0
R1 = L1
R2 = L2
R3 = L3
R4 = L4
R5 = L5
R6 = L6
L7 = Idris(301)
R0 = L7
R1 = L5
GoTo Idris3

' VBA.Socket.{listen1}
Case 243
Idris243:
L0 = R0
Idris = Array(0,L0)

' Main.{main1}
Case 135
Idris135:
L0 = R0
Idris = Array(65634)

' VBA.Socket.{peekSockAddr1}
Case 258
Idris258:
L0 = R0
Idris = Array(65773)

' VBA.Socket.{pokeSockAddr1}
Case 275
Idris275:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = 0
L5 = 0
L6 = 0
L7 = CLngLng(&H2^)
R0 = L0
R1 = L7
L7 = Idris(57)
R0 = L1
L8 = Idris(33)
L7 = Array(65720,L7,L8)
L8 = Array(65800,L0,L2)
Idris = Array(65811,L4,L5,L6,L7,L8)

' Prelude.{putStr1}
Case 184
Idris184:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(0)
Idris = Array(65812,L1,L2,L3)

' VBA.Socket.{setNonBlocking1}
Case 277
Idris277:
L0 = R0
L1 = R1
L2 = 0
L3 = Array(65816)
R0 = L2
R1 = L3
L2 = Idris(5)
R0 = L2
R1 = L0
L2 = Idris(3)
L3 = CLng(&H0&)
R0 = L2
R1 = L3
L2 = Idris(3)
Select Case L2(0)
Case 0
L2 = Array(1)
Case 1
L2 = Array(0)
End Select
Select Case L2(0)
Case 0
L3 = 0
L4 = 0
L5 = Array(0)
L5 = Array(1,L5)
Idris = Array(65812,L3,L4,L5)
Case 1
L3 = 0
L4 = 0
L5 = 0
L6 = Array(65802)
L7 = Array(65725)
R0 = L3
R1 = L4
R2 = L5
R3 = L6
R4 = L7
GoTo Idris313
End Select

' Prelude.{showLitChar1}
Case 187
Idris187:
L0 = R0
L1 = "\b"
Idris = Idris_Append(L1, L0)

' VBA.Socket.{socket1}
Case 283
Idris283:
L0 = R0
Idris = Array(0,L0)

' {unsafePerformIO1}
Case 115
Idris115:
L0 = R0
L1 = R1
L2 = R2
Idris = Array(65815)

' Prelude.Classes.{Char instance of Prelude.Classes.Ord2}
Case 164
Idris164:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(109)
R0 = L2
R1 = L3
L2 = Idris(23)
R0 = L2
R1 = L0
L2 = Idris(3)
R0 = L2
R1 = L1
L2 = Idris(3)
Select Case L2(0)
Case 0
Idris = Array(1)
Case Else
Idris = Array(0)
End Select

' Prelude.Classes.{Int instance of Prelude.Classes.Ord2}
Case 170
Idris170:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(314)
R0 = L2
R1 = L3
L2 = Idris(23)
R0 = L2
R1 = L0
L2 = Idris(3)
R0 = L2
R1 = L1
L2 = Idris(3)
Select Case L2(0)
Case 0
Idris = Array(1)
Case Else
Idris = Array(0)
End Select

' Prelude.Monad.{VBA.Except.VBAExcept e instance of Prelude.Monad.Monad, method >>=2}
Case 177
Idris177:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
Idris = Array(65811,L2,L3,L4,L0,L1)

' VBA.Socket.{accept2}
Case 218
Idris218:
L0 = R0
Idris = Array(65738)

' VBA.Socket.{bind2}
Case 230
Idris230:
L0 = R0
L1 = R1
L2 = CLng(&Hffffffff&)
L2 = (L0 = L2)
Select Case L2
Case CLng(&H0&)
L2 = Array(0)
Case Else
L2 = Array(1)
End Select
Select Case L2(0)
Case 0
L3 = 0
L4 = 0
L5 = Array(0)
L5 = Array(1,L5)
Idris = Array(65812,L3,L4,L5)
Case 1
L3 = 0
L4 = 0
L5 = 0
L6 = Array(65755)
L7 = Array(65725)
R0 = L3
R1 = L4
R2 = L5
R3 = L6
R4 = L7
GoTo Idris313
End Select

' {io_bind2}
Case 38
Idris38:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
Idris = Array(65814,L0,L1,L2,L3,L4,L5)

' VBA.Socket.{listen2}
Case 244
Idris244:
L0 = R0
L1 = CLng(&Hffffffff&)
L1 = (L0 = L1)
Select Case L1
Case CLng(&H0&)
L1 = Array(0)
Case Else
L1 = Array(1)
End Select
Select Case L1(0)
Case 0
L2 = 0
L3 = 0
L4 = Array(0)
L4 = Array(1,L4)
Idris = Array(65812,L2,L3,L4)
Case 1
L2 = 0
L3 = 0
L4 = 0
L5 = Array(65769)
L6 = Array(65725)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
R4 = L6
GoTo Idris313
End Select

' Main.{main2}
Case 146
Idris146:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
R0 = L2
R1 = L3
R2 = L4
R3 = L0
R4 = L1
GoTo Idris315

' VBA.Socket.{peekSockAddr2}
Case 266
Idris266:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
Idris = Array(65811,L2,L3,L4,L0,L1)

' VBA.Socket.{setNonBlocking2}
Case 278
Idris278:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = 0
R0 = L0
L6 = Idris(98)
L7 = CInt(&H8)
L6 = Idris_LShr16(L6, L7)
L6 = CByte(L6 And &HFF)
R0 = L6
L6 = Idris(14)
R0 = L0
L7 = Idris(98)
L7 = CByte(L7 And &HFF)
R0 = L7
L7 = Idris(14)
L6 = Idris_Append(L6, L7)
L7 = Idris_UTrunc_32_16(L0)
L8 = CInt(&H8)
L7 = Idris_LShr16(L7, L8)
L7 = CByte(L7 And &HFF)
R0 = L7
L7 = Idris(14)
L8 = Idris_UTrunc_32_16(L0)
L8 = CByte(L8 And &HFF)
R0 = L8
L8 = Idris(14)
L7 = Idris_Append(L7, L8)
L6 = Idris_Append(L6, L7)
R0 = L5
R1 = L6
L5 = Idris(47)
L6 = Array(65803,L0)
Idris = Array(65811,L2,L3,L4,L5,L6)

' Prelude.{showLitChar2}
Case 188
Idris188:
L0 = R0
L1 = "\t"
Idris = Idris_Append(L1, L0)

' VBA.Socket.{socket2}
Case 284
Idris284:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = CLngLng(&Hffffffffffffffff^)
L4 = (L3 = L4)
Select Case L4
Case CLng(&H0&)
L4 = Array(0)
Case Else
L4 = Array(1)
End Select
Select Case L4(0)
Case 0
L5 = 0
L6 = 0
L7 = Array(0,L3,L0,L1,L2)
L7 = Array(1,L7)
Idris = Array(65812,L5,L6,L7)
Case 1
L5 = 0
L6 = 0
L7 = 0
L8 = Array(65809)
L9 = Array(65725)
R0 = L5
R1 = L6
R2 = L7
R3 = L8
R4 = L9
GoTo Idris313
End Select

' Prelude.Classes.{Char instance of Prelude.Classes.Ord3}
Case 165
Idris165:
L0 = R0
Idris = Array(65677,L0)

' Prelude.Classes.{Int instance of Prelude.Classes.Ord3}
Case 171
Idris171:
L0 = R0
Idris = Array(65683,L0)

' Prelude.Monad.{VBA.Except.VBAExcept e instance of Prelude.Monad.Monad, method >>=3}
Case 178
Idris178:
L0 = R0
Idris = Array(65690,L0)

' VBA.Socket.{accept3}
Case 219
Idris219:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
Idris = Array(65811,L2,L3,L4,L0,L1)

' VBA.Socket.{bind3}
Case 231
Idris231:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = Array(65715,L0)
L6 = Array(65756,L1)
Idris = Array(65811,L2,L3,L4,L5,L6)

' Main.{main3}
Case 148
Idris148:
L0 = R0
Idris = Array(65656,L0)

' VBA.Socket.{peekSockAddr3}
Case 267
Idris267:
L0 = R0
Idris = Array(65792,L0)

' VBA.Socket.{setNonBlocking3}
Case 279
Idris279:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = Array(65715,L0)
L6 = Array(65804,L1)
Idris = Array(65811,L2,L3,L4,L5,L6)

' Prelude.{showLitChar3}
Case 189
Idris189:
L0 = R0
L1 = "\n"
Idris = Idris_Append(L1, L0)

' Prelude.Classes.{Char instance of Prelude.Classes.Ord4}
Case 166
Idris166:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(109)
R0 = L2
R1 = L3
L2 = Idris(23)
R0 = L2
R1 = L0
L2 = Idris(3)
R0 = L2
R1 = L1
L2 = Idris(3)
Select Case L2(0)
Case 2
Idris = Array(1)
Case Else
Idris = Array(0)
End Select

' Prelude.Classes.{Int instance of Prelude.Classes.Ord4}
Case 172
Idris172:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(314)
R0 = L2
R1 = L3
L2 = Idris(23)
R0 = L2
R1 = L0
L2 = Idris(3)
R0 = L2
R1 = L1
L2 = Idris(3)
Select Case L2(0)
Case 2
Idris = Array(1)
Case Else
Idris = Array(0)
End Select

' Prelude.Monad.{VBA.Except.VBAExcept e instance of Prelude.Monad.Monad, method >>=4}
Case 179
Idris179:
L0 = R0
Idris = Array(65691)

' VBA.Socket.{accept4}
Case 220
Idris220:
L0 = R0
Idris = Array(65745,L0)

' VBA.Socket.{bind4}
Case 232
Idris232:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
L6 = Array(65754,L0,L1)
L7 = Array(65757,L1)
Idris = Array(65811,L3,L4,L5,L6,L7)

' Main.{main4}
Case 149
Idris149:
L0 = R0
Idris = Array(65658)

' VBA.Socket.{peekSockAddr4}
Case 268
Idris268:
L0 = R0
Idris = Array(65793)

' VBA.Socket.{setNonBlocking4}
Case 280
Idris280:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
Select Case L0(0)
Case 0
L6 = L0(1)
L7 = L0(2)
L8 = L0(3)
L9 = L0(4)
L6 = L6
End Select
L7 = CLng(&H5421&)
L6 = Array(65726,L6,L7,L1)
L7 = Array(65805,L1)
Idris = Array(65811,L3,L4,L5,L6,L7)

' Prelude.{showLitChar4}
Case 190
Idris190:
L0 = R0
L1 = "\v"
Idris = Idris_Append(L1, L0)

' Prelude.Classes.{Char instance of Prelude.Classes.Ord5}
Case 167
Idris167:
L0 = R0
Idris = Array(65679,L0)

' Prelude.Classes.{Int instance of Prelude.Classes.Ord5}
Case 173
Idris173:
L0 = R0
Idris = Array(65685,L0)

' Prelude.Monad.{VBA.Except.VBAExcept e instance of Prelude.Monad.Monad, method >>=5}
Case 180
Idris180:
L0 = R0
Idris = Array(65692)

' VBA.Socket.{accept5}
Case 221
Idris221:
L0 = R0
Idris = Array(65746)

' VBA.Socket.{bind5}
Case 233
Idris233:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
Select Case L0(0)
Case 0
L6 = L0(1)
L7 = L0(2)
L8 = L0(3)
L9 = L0(4)
L6 = L7
End Select
L7 = CLng(&H0&)
L6 = Array(0,L6,L7,L1)
R0 = L2
R1 = L6
L6 = Idris(61)
L7 = Array(65758,L0,L2)
Idris = Array(65811,L3,L4,L5,L6,L7)

' Main.{main5}
Case 150
Idris150:
L0 = R0
Idris = Array(65659)

' VBA.Socket.{peekSockAddr5}
Case 269
Idris269:
L0 = R0
Idris = Array(65794)

' VBA.Socket.{setNonBlocking5}
Case 281
Idris281:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = CLng(&H1&)
L5 = Array(65721,L1,L5)
L6 = Array(65806,L0,L1)
Idris = Array(65811,L2,L3,L4,L5,L6)

' Prelude.{showLitChar5}
Case 191
Idris191:
L0 = R0
L1 = "\f"
Idris = Idris_Append(L1, L0)

' Prelude.Monad.{VBA.Except.VBAExcept e instance of Prelude.Monad.Monad, method >>=6}
Case 181
Idris181:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = 0
L4 = 0
L5 = Array(65689)
L6 = Array(65693)
L5 = Array(0,L5,L6)
Select Case L5(0)
Case 0
L6 = L5(1)
L7 = L5(2)
L5 = L6
End Select
R0 = L3
R1 = L4
R2 = L5
L3 = Idris(102)
L4 = Array(0,L2)
R0 = L3
R1 = L4
GoTo Idris3
Case 1
L2 = L1(1)
R0 = L0
R1 = L2
GoTo Idris3
End Select

' VBA.Socket.{accept6}
Case 222
Idris222:
L0 = R0
Idris = Array(65747)

' Main.{main6}
Case 151
Idris151:
L0 = R0
L1 = 0
L2 = 0
R0 = L1
R1 = L2
R2 = L0
GoTo Idris304

' VBA.Socket.{peekSockAddr6}
Case 270
Idris270:
L0 = R0
L1 = 0
L2 = 0
Idris = Array(65812,L1,L2,L0)

' Prelude.{showLitChar6}
Case 192
Idris192:
L0 = R0
L1 = "\r"
Idris = Idris_Append(L1, L0)

' VBA.Socket.{accept7}
Case 223
Idris223:
L0 = R0
L1 = 0
L2 = 0
Idris = Array(65812,L1,L2,L0)

' Main.{main7}
Case 152
Idris152:
L0 = R0
Idris = Array(65661)

' VBA.Socket.{peekSockAddr7}
Case 271
Idris271:
L0 = R0
Idris = Array(65796)

' Prelude.{showLitChar7}
Case 193
Idris193:
L0 = R0
L1 = CInt(&H48)
L1 = (L0 = L1)
Select Case L1
Case CLng(&H0&)
Idris = Array(0)
Case Else
Idris = Array(1)
End Select

' VBA.Socket.{accept8}
Case 224
Idris224:
L0 = R0
Idris = Array(65749)

' Main.{main8}
Case 153
Idris153:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
R0 = L2
R1 = L3
R2 = L4
R3 = L0
R4 = L1
GoTo Idris315

' VBA.Socket.{peekSockAddr8}
Case 272
Idris272:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
Idris = Array(65811,L2,L3,L4,L0,L1)

' Prelude.{showLitChar8}
Case 194
Idris194:
L0 = R0
L1 = "\\"
Idris = Idris_Append(L1, L0)

' VBA.Socket.{accept9}
Case 225
Idris225:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
Idris = Array(65811,L2,L3,L4,L0,L1)

' Main.{main9}
Case 154
Idris154:
L0 = R0
Idris = Array(65663,L0)

' VBA.Socket.{peekSockAddr9}
Case 273
Idris273:
L0 = R0
Idris = Array(65798,L0)

' Prelude.{showLitChar9}
Case 195
Idris195:
L0 = R0
L1 = "\DEL"
Idris = Idris_Append(L1, L0)

' VBA.Socket.{accept10}
Case 202
Idris202:
L0 = R0
Idris = Array(65751,L0)

' Main.{main10}
Case 125
Idris125:
L0 = R0
Idris = Array(65664)

' VBA.Socket.{peekSockAddr10}
Case 248
Idris248:
L0 = R0
Idris = Array(65799)

' Prelude.{showLitChar10}
Case 186
Idris186:
L0 = R0
L1 = R1
Idris = Idris_Append(L0, L1)

' VBA.Socket.{accept11}
Case 203
Idris203:
L0 = R0
Idris = Array(65728)

' Main.{main11}
Case 126
Idris126:
L0 = R0
Idris = Array(65635)

' VBA.Socket.{peekSockAddr11}
Case 249
Idris249:
L0 = R0
Idris = Array(65774)

' VBA.Socket.{accept12}
Case 204
Idris204:
L0 = R0
Idris = Array(65729)

' Main.{main12}
Case 127
Idris127:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = "Client socket closed" & ChrW(10)
R0 = L3
R1 = L4
L3 = Idris(47)
R0 = L1
R1 = L2
R2 = L3
GoTo Idris119

' VBA.Socket.{peekSockAddr12}
Case 250
Idris250:
L0 = R0
L1 = 0
L2 = 0
Idris = Array(65812,L1,L2,L0)

' VBA.Socket.{accept13}
Case 205
Idris205:
L0 = R0
L1 = 0
L2 = 0
Idris = Array(65812,L1,L2,L0)

' Main.{main13}
Case 128
Idris128:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = Array(65662)
L6 = Array(65636)
L5 = Array(0,L5,L6)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
L2 = Idris(7)
L3 = 0
L4 = 0
Select Case L0(0)
Case 0
L5 = L0(1)
L6 = L0(2)
L7 = L0(3)
L8 = L0(4)
L5 = L5
End Select
L5 = Array(65724,L5)
R0 = L3
R1 = L4
R2 = L5
L3 = Idris(119)
R0 = L2
R1 = L3
L2 = Idris(3)
L3 = Array(65637)
R0 = L2
R1 = L3
GoTo Idris3

' VBA.Socket.{peekSockAddr13}
Case 251
Idris251:
L0 = R0
Idris = Array(65776)

' VBA.Socket.{accept14}
Case 206
Idris206:
L0 = R0
Idris = Array(65731)

' Main.{main14}
Case 129
Idris129:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = "Socket closed" & ChrW(10)
R0 = L3
R1 = L4
L3 = Idris(47)
R0 = L1
R1 = L2
R2 = L3
GoTo Idris119

' VBA.Socket.{peekSockAddr14}
Case 252
Idris252:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
Idris = Array(65811,L2,L3,L4,L0,L1)

' VBA.Socket.{accept15}
Case 207
Idris207:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
Idris = Array(65811,L2,L3,L4,L0,L1)

' Main.{main15}
Case 130
Idris130:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = 0
L6 = 0
Select Case L0(0)
Case 0
L7 = L0(1)
L8 = L0(2)
L9 = L0(3)
L10 = L0(4)
L7 = L7
End Select
L7 = Array(65724,L7)
R0 = L5
R1 = L6
R2 = L7
L5 = Idris(119)
L6 = Array(65639)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
R4 = L6
GoTo Idris315

' VBA.Socket.{peekSockAddr15}
Case 253
Idris253:
L0 = R0
Idris = Array(65778,L0)

' VBA.Socket.{accept16}
Case 208
Idris208:
L0 = R0
Idris = Array(65733,L0)

' Main.{main16}
Case 131
Idris131:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
Select Case L1(0)
Case 1
L5 = L1(1)
Select Case L5(0)
Case 0
L6 = L5(1)
L7 = L5(2)
L8 = 0
L9 = 0
L10 = 0
L11 = Array(65645)
L12 = Array(65660)
L11 = Array(0,L11,L12)
R0 = L8
R1 = L9
R2 = L10
R3 = L11
L8 = Idris(7)
L9 = 0
L10 = 0
L11 = 0
L12 = "Accepted client "
R0 = L7
L13 = Idris(316)
L12 = Idris_Append(L12, L13)
L13 = ChrW(10)
L12 = Idris_Append(L12, L13)
R0 = L11
R1 = L12
L11 = Idris(47)
R0 = L9
R1 = L10
R2 = L11
L9 = Idris(119)
R0 = L8
R1 = L9
L8 = Idris(3)
L9 = Array(65638,L6)
R0 = L8
R1 = L9
L5 = Idris(3)
End Select
Case 0
L5 = 0
L6 = 0
L7 = 0
L8 = "Did not accept client" & ChrW(10)
R0 = L7
R1 = L8
L7 = Idris(47)
R0 = L5
R1 = L6
R2 = L7
L5 = Idris(119)
End Select
L6 = Array(65640,L0)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
R4 = L6
GoTo Idris315

' VBA.Socket.{peekSockAddr16}
Case 254
Idris254:
L0 = R0
Idris = Array(65779)

' VBA.Socket.{accept17}
Case 209
Idris209:
L0 = R0
Idris = Array(65734)

' Main.{main17}
Case 132
Idris132:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
R0 = L0
L5 = Idris(10)
L6 = Array(65641,L0)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
R4 = L6
GoTo Idris315

' VBA.Socket.{peekSockAddr17}
Case 255
Idris255:
L0 = R0
Idris = Array(65780)

' VBA.Socket.{accept18}
Case 210
Idris210:
L0 = R0
Idris = Array(65735)

' Main.{main18}
Case 133
Idris133:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = 0
L6 = 0
L7 = 0
L8 = "Listening on port 8080" & ChrW(10)
R0 = L7
R1 = L8
L7 = Idris(47)
R0 = L5
R1 = L6
R2 = L7
L5 = Idris(119)
L6 = Array(65642,L0)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
R4 = L6
GoTo Idris315

' VBA.Socket.{peekSockAddr18}
Case 256
Idris256:
L0 = R0
L1 = 0
L2 = 0
Idris = Array(65812,L1,L2,L0)

' VBA.Socket.{accept19}
Case 211
Idris211:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(65732)
L4 = Array(65736)
L3 = Array(0,L3,L4)
Select Case L3(0)
Case 0
L4 = L3(1)
L5 = L3(2)
L3 = L4
End Select
R0 = L1
R1 = L2
R2 = L3
L1 = Idris(102)
L2 = Array(0)
L2 = Array(1,L2)
R0 = L1
R1 = L2
GoTo Idris3

' Main.{main19}
Case 134
Idris134:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = CLng(&H40&)
R0 = L0
R1 = L5
L5 = Idris(45)
L6 = Array(65643,L0)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
R4 = L6
GoTo Idris315

' VBA.Socket.{peekSockAddr19}
Case 257
Idris257:
L0 = R0
Idris = Array(65782)

' VBA.Socket.{accept20}
Case 213
Idris213:
L0 = R0
Idris = Array(0,L0)

' Main.{main20}
Case 136
Idris136:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = 0
L6 = 0
L7 = 0
L8 = "Bound to port 8080" & ChrW(10)
R0 = L7
R1 = L8
L7 = Idris(47)
R0 = L5
R1 = L6
R2 = L7
L5 = Idris(119)
L6 = Array(65644,L0)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
R4 = L6
GoTo Idris315

' VBA.Socket.{peekSockAddr20}
Case 259
Idris259:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
Idris = Array(65811,L2,L3,L4,L0,L1)

' VBA.Socket.{accept21}
Case 214
Idris214:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = CLngLng(&Hffffffffffffffff^)
L4 = (L0 = L4)
Select Case L4
Case CLng(&H0&)
L4 = Array(0)
Case Else
L4 = Array(1)
End Select
Select Case L4(0)
Case 0
Select Case L1(0)
Case 1
L5 = L1(1)
L6 = 0
L7 = 0
L8 = Array(65744)
L9 = Array(65748)
L8 = Array(0,L8,L9)
Select Case L8(0)
Case 0
L9 = L8(1)
L10 = L8(2)
L8 = L9
End Select
R0 = L6
R1 = L7
R2 = L8
L6 = Idris(102)
Select Case L5(0)
Case 0
L7 = L5(1)
L8 = L5(2)
L9 = L5(3)
L7 = L7
End Select
Select Case L2(0)
Case 0
L8 = L2(1)
L9 = L2(2)
L10 = L2(3)
L11 = L2(4)
L8 = L10
End Select
Select Case L2(0)
Case 0
L9 = L2(1)
L10 = L2(2)
L11 = L2(3)
L12 = L2(4)
L9 = L12
End Select
L7 = Array(0,L0,L7,L8,L9)
L7 = Array(0,L7,L5)
L7 = Array(1,L7)
L7 = Array(1,L7)
R0 = L6
R1 = L7
GoTo Idris3
Case 0
L5 = 0
L6 = 0
L7 = 0
L8 = Array(65750)
L9 = Array(65730)
L8 = Array(0,L8,L9)
R0 = L5
R1 = L6
R2 = L7
R3 = L8
L5 = Idris(7)
L6 = Array(65724,L0)
R0 = L5
R1 = L6
L5 = Idris(3)
L6 = Array(65737)
R0 = L5
R1 = L6
GoTo Idris3
End Select
Case 1
L5 = 0
L6 = 0
L7 = 0
L8 = Array(65739)
L9 = Array(65725)
R0 = L5
R1 = L6
R2 = L7
R3 = L8
R4 = L9
GoTo Idris313
End Select

' Main.{main21}
Case 137
Idris137:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = CInt(&H1f90)
R0 = L0
R1 = L5
L5 = Idris(18)
L6 = Array(65646,L0)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
R4 = L6
GoTo Idris315

' VBA.Socket.{peekSockAddr21}
Case 260
Idris260:
L0 = R0
Idris = Array(65785,L0)

' VBA.Socket.{accept22}
Case 215
Idris215:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = 0
L5 = 0
L6 = 0
L7 = Array(65715,L0)
L8 = Array(65740,L1,L3,L2)
Idris = Array(65811,L4,L5,L6,L7,L8)

' Main.{main22}
Case 138
Idris138:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = 0
L6 = 0
L7 = 0
L8 = "Set to non-blocking" & ChrW(10)
R0 = L7
R1 = L8
L7 = Idris(47)
R0 = L5
R1 = L6
R2 = L7
L5 = Idris(119)
L6 = Array(65647,L0)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
R4 = L6
GoTo Idris315

' VBA.Socket.{peekSockAddr22}
Case 261
Idris261:
L0 = R0
Idris = Array(65786)

' VBA.Socket.{accept23}
Case 216
Idris216:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
R0 = L0
L6 = Idris(56)
L7 = Array(65741,L0,L2,L1)
Idris = Array(65811,L3,L4,L5,L6,L7)

' Main.{main23}
Case 139
Idris139:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
R0 = L0
L5 = Idris(105)
L6 = Array(65648,L0)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
R4 = L6
GoTo Idris315

' VBA.Socket.{peekSockAddr23}
Case 262
Idris262:
L0 = R0
Idris = Array(65787)

' VBA.Socket.{accept24}
Case 217
Idris217:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = Array(65727,L0,L1)
L6 = Array(65742,L1,L0)
Idris = Array(65811,L2,L3,L4,L5,L6)

' Main.{main24}
Case 140
Idris140:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = 0
L5 = 0
L6 = 0
R0 = L0
L7 = Idris(317)
R0 = L6
R1 = L7
L6 = Idris(47)
R0 = L4
R1 = L5
R2 = L6
L4 = Idris(119)
L5 = Array(65649,L0)
R0 = L1
R1 = L2
R2 = L3
R3 = L4
R4 = L5
GoTo Idris315

' VBA.Socket.{peekSockAddr24}
Case 263
Idris263:
L0 = R0
L1 = R1
L2 = R2
L3 = CLng(L0 And &HFF^)
R0 = L3
L3 = Idris(28)
Select Case L3(0)
Case 1
L4 = L3(1)
L5 = 0
L6 = 0
L7 = Array(65777)
L8 = Array(65781)
L7 = Array(0,L7,L8)
Select Case L7(0)
Case 0
L8 = L7(1)
L9 = L7(2)
L7 = L8
End Select
R0 = L5
R1 = L6
R2 = L7
L5 = Idris(102)
R0 = L2
L6 = Idris(50)
R0 = L1
L7 = Idris(51)
L6 = Array(0,L4,L6,L7)
L6 = Array(1,L6)
R0 = L5
R1 = L6
GoTo Idris3
Case 0
L4 = 0
L5 = 0
L6 = Array(65783)
L7 = Array(65788)
L6 = Array(0,L6,L7)
Select Case L6(0)
Case 0
L7 = L6(1)
L8 = L6(2)
L6 = L7
End Select
R0 = L4
R1 = L5
R2 = L6
L4 = Idris(102)
L5 = Array(0)
R0 = L4
R1 = L5
GoTo Idris3
End Select

' Main.{main25}
Case 141
Idris141:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(65631)
L4 = 0
L5 = 0
L6 = 0
L7 = Array(1)
L8 = Array(1)
L9 = CLng(&H0&)
R0 = L7
R1 = L8
R2 = L9
L7 = Idris(111)
L8 = Array(65650)
R0 = L4
R1 = L5
R2 = L6
R3 = L7
R4 = L8
L4 = Idris(315)
R0 = L1
R1 = L2
R2 = L3
R3 = L4
GoTo Idris117

' VBA.Socket.{peekSockAddr25}
Case 264
Idris264:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
L6 = Array(65797)
L7 = Array(65775)
L6 = Array(0,L6,L7)
R0 = L3
R1 = L4
R2 = L5
R3 = L6
L3 = Idris(7)
L4 = CLngLng(&H4^)
R0 = L0
R1 = L4
L4 = Idris(57)
L4 = Array(65717,L4)
R0 = L3
R1 = L4
L3 = Idris(3)
L4 = Array(65789,L1,L2)
R0 = L3
R1 = L4
GoTo Idris3

' Main.{main26}
Case 142
Idris142:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = 0
L6 = "hip4 = "
R0 = L0
L7 = Idris(318)
L6 = Idris_Append(L6, L7)
L7 = ChrW(10)
L6 = Idris_Append(L6, L7)
R0 = L5
R1 = L6
L5 = Idris(47)
L6 = Array(65651)
Idris = Array(65811,L2,L3,L4,L5,L6)

' VBA.Socket.{peekSockAddr26}
Case 265
Idris265:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = Array(65784)
L6 = Array(65795)
L5 = Array(0,L5,L6)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
L2 = Idris(7)
L3 = CLngLng(&H2^)
R0 = L0
R1 = L3
L3 = Idris(57)
L3 = Array(65716,L3)
R0 = L2
R1 = L3
L2 = Idris(3)
L3 = Array(65790,L0,L1)
R0 = L2
R1 = L3
GoTo Idris3

' Main.{main27}
Case 143
Idris143:
L0 = R0
L1 = R1
L2 = R2
R0 = L0
L3 = Idris(35)
R0 = L1
L4 = Idris(35)
L5 = 0
L6 = 0
L7 = 0
L8 = 0
L9 = "nip4 = "
R0 = L3
L10 = Idris(318)
L9 = Idris_Append(L9, L10)
L10 = ChrW(10)
L9 = Idris_Append(L9, L10)
R0 = L8
R1 = L9
L8 = Idris(47)
L9 = Array(65652,L4)
Idris = Array(65811,L5,L6,L7,L8,L9)

' Main.{main28}
Case 144
Idris144:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
L6 = 0
L7 = "n32 = "
R0 = L0
L8 = Idris(98)
L9 = CInt(&H8)
L8 = Idris_LShr16(L8, L9)
L8 = CByte(L8 And &HFF)
R0 = L8
L8 = Idris(14)
R0 = L0
L9 = Idris(98)
L9 = CByte(L9 And &HFF)
R0 = L9
L9 = Idris(14)
L8 = Idris_Append(L8, L9)
L9 = Idris_UTrunc_32_16(L0)
L10 = CInt(&H8)
L9 = Idris_LShr16(L9, L10)
L9 = CByte(L9 And &HFF)
R0 = L9
L9 = Idris(14)
L10 = Idris_UTrunc_32_16(L0)
L10 = CByte(L10 And &HFF)
R0 = L10
L10 = Idris(14)
L9 = Idris_Append(L9, L10)
L8 = Idris_Append(L8, L9)
L7 = Idris_Append(L7, L8)
L8 = ChrW(10)
L7 = Idris_Append(L7, L8)
R0 = L6
R1 = L7
L6 = Idris(47)
L7 = Array(65653,L1,L0)
Idris = Array(65811,L3,L4,L5,L6,L7)

' Main.{main29}
Case 145
Idris145:
L0 = R0
L1 = R1
L2 = R2
R0 = L0
L3 = Idris(33)
R0 = L1
L4 = Idris(31)
L5 = 0
L6 = 0
L7 = 0
L8 = 0
L9 = "n16 = "
L10 = CInt(&H8)
L10 = Idris_LShr16(L3, L10)
L10 = CByte(L10 And &HFF)
R0 = L10
L10 = Idris(14)
L11 = CByte(L3 And &HFF)
R0 = L11
L11 = Idris(14)
L10 = Idris_Append(L10, L11)
L9 = Idris_Append(L9, L10)
L10 = ChrW(10)
L9 = Idris_Append(L9, L10)
R0 = L8
R1 = L9
L8 = Idris(47)
L9 = Array(65654,L4,L1)
Idris = Array(65811,L5,L6,L7,L8,L9)

' Main.{main30}
Case 147
Idris147:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
L6 = 0
L7 = "h32 = "
R0 = L0
L8 = Idris(98)
L9 = CInt(&H8)
L8 = Idris_LShr16(L8, L9)
L8 = CByte(L8 And &HFF)
R0 = L8
L8 = Idris(14)
R0 = L0
L9 = Idris(98)
L9 = CByte(L9 And &HFF)
R0 = L9
L9 = Idris(14)
L8 = Idris_Append(L8, L9)
L9 = Idris_UTrunc_32_16(L0)
L10 = CInt(&H8)
L9 = Idris_LShr16(L9, L10)
L9 = CByte(L9 And &HFF)
R0 = L9
L9 = Idris(14)
L10 = Idris_UTrunc_32_16(L0)
L10 = CByte(L10 And &HFF)
R0 = L10
L10 = Idris(14)
L9 = Idris_Append(L9, L10)
L8 = Idris_Append(L8, L9)
L7 = Idris_Append(L7, L8)
L8 = ChrW(10)
L7 = Idris_Append(L7, L8)
R0 = L6
R1 = L7
L6 = Idris(47)
L7 = Array(65655,L1,L0)
Idris = Array(65811,L3,L4,L5,L6,L7)

' Prelude.Bits.b8ToString, c1
Case 15
Idris15:
L0 = R0
L1 = 0
L2 = CByte(&H4)
L2 = Idris_LShr8(L0, L2)
L3 = CByte(&Hf)
L2 = (L2 And L3)
R0 = L1
R1 = L2
GoTo Idris16

' Prelude.Bits.b8ToString, getDigit
Case 16
Idris16:
L0 = R0
L1 = R1
L2 = CLng(L1 And &HFF^)
L3 = CLng(&H0&)
R0 = L2
R1 = L3
L3 = Idris(319)
Select Case L3(0)
Case 0
L3 = Array(0)
Case 1
R0 = L2
L3 = Idris(310)
End Select
Select Case L3(0)
Case 0
L4 = CLng(&Ha&)
R0 = L2
R1 = L4
L4 = Idris(319)
Select Case L4(0)
Case 0
L4 = Array(0)
Case 1
R0 = L2
L4 = Idris(289)
End Select
Select Case L4(0)
Case 0
Idris = CInt(&H3f)
Case 1
L5 = CInt(&H41)
L6 = CLng(&Ha&)
L6 = (L2 - L6)
L6 = CInt(L6)
Idris = (L5 + L6)
End Select
Case 1
L4 = CInt(&H30)
L5 = CInt(L2)
Idris = (L4 + L5)
End Select

' Prelude.Nat.toIntNat, toIntNat'
Case 311
Idris311:
L0 = R0
L1 = R1
L2 = R2
Select Case L1
Case CLngLng(&H0^)
Idris = L2
Case Else
L3 = CLngLng(&H1^)
L3 = (L1 - L3)
L4 = 0
L5 = CLng(&H1&)
L5 = (L2 + L5)
R0 = L4
R1 = L3
R2 = L5
GoTo Idris311
End Select

' Decidable.Equality.Decidable.Equality.Char instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 320
Idris320:
Idris = 0

' Decidable.Equality.Decidable.Equality.Int instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 321
Idris321:
Idris = 0

' Decidable.Equality.Decidable.Equality.Integer instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 322
Idris322:
Idris = 0

' Decidable.Equality.Decidable.Equality.String instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 323
Idris323:
Idris = 0

' Prelude.Applicative.VBA.Except.VBAExcept e instance of Prelude.Applicative.Applicative, method pure
Case 304
Idris304:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = Array(1,L2)
Idris = Array(65812,L3,L4,L5)

' Prelude.Cast.Util.Nat, Bits32 instance of Prelude.Cast.Cast, method cast
Case 295
Idris295:
L0 = 0
L1 = 0
L2 = 0
L3 = Array(65667)
L4 = Array(65668)
Idris = Array(65665,L0,L1,L2,L3,L4)

' Prelude.Cast.Util.Nat, Bits64 instance of Prelude.Cast.Cast, method cast
Case 13
Idris13:
L0 = 0
L1 = 0
L2 = 0
L3 = Array(65669)
L4 = Array(65670)
Idris = Array(65665,L0,L1,L2,L3,L4)

' Prelude.Cast.Util.Nat, Bits8 instance of Prelude.Cast.Cast, method cast
Case 312
Idris312:
L0 = 0
L1 = 0
L2 = 0
L3 = Array(65671)
L4 = Array(65672)
Idris = Array(65665,L0,L1,L2,L3,L4)

' Decidable.Equality.Decidable.Equality.Bool instance of Decidable.Equality.DecEq, method decEq
Case 113
Idris113:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
Select Case L0(0)
Case 0
Idris = Array(0)
Case 1
Idris = Array(1)
End Select
Case 1
Select Case L0(0)
Case 0
Idris = Array(1)
Case 1
Idris = Array(0)
End Select
End Select

' Prelude.Functor.Prelude.IO' ffi instance of Prelude.Functor.Functor, method map
Case 313
Idris313:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = 0
L6 = 0
L7 = 0
L8 = Array(65687,L3)
Idris = Array(65811,L5,L6,L7,L4,L8)

' Prelude.Monad.VBA.Except.VBAExcept e instance of Prelude.Monad.Monad, method >>=
Case 315
Idris315:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = 0
L6 = 0
L7 = 0
L8 = Array(65694,L4)
Idris = Array(65811,L5,L6,L7,L3,L8)

' Prelude.Classes.Prelude.Classes.Char instance of Prelude.Classes.Ord, method <=
Case 302
Idris302:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(109)
R0 = L2
R1 = L3
L2 = Idris(4)
R0 = L2
R1 = L0
L2 = Idris(3)
R0 = L2
R1 = L1
L2 = Idris(3)
Select Case L2(0)
Case 0
R0 = L0
R1 = L1
GoTo Idris291
Case 1
Idris = Array(1)
End Select

' Prelude.Classes.Prelude.Classes.Char instance of Prelude.Classes.Ord, method >=
Case 43
Idris43:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(109)
R0 = L2
R1 = L3
L2 = Idris(6)
R0 = L2
R1 = L0
L2 = Idris(3)
R0 = L2
R1 = L1
L2 = Idris(3)
Select Case L2(0)
Case 0
R0 = L0
R1 = L1
GoTo Idris292
Case 1
Idris = Array(1)
End Select

' Prelude.Classes.Prelude.Classes.Char instance of Prelude.Classes.Ord, method compare
Case 287
Idris287:
L0 = R0
L1 = R1
L2 = (L0 = L1)
Select Case L2
Case CLng(&H0&)
L2 = Array(0)
Case Else
L2 = Array(1)
End Select
Select Case L2(0)
Case 0
L3 = (L0 < L1)
Select Case L3
Case CLng(&H0&)
L3 = Array(0)
Case Else
L3 = Array(1)
End Select
Select Case L3(0)
Case 0
Idris = Array(2)
Case 1
Idris = Array(0)
End Select
Case 1
Idris = Array(1)
End Select

' Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=
Case 290
Idris290:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(314)
R0 = L2
R1 = L3
L2 = Idris(4)
R0 = L2
R1 = L0
L2 = Idris(3)
R0 = L2
R1 = L1
L2 = Idris(3)
Select Case L2(0)
Case 0
R0 = L0
R1 = L1
GoTo Idris293
Case 1
Idris = Array(1)
End Select

' Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method >=
Case 319
Idris319:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(314)
R0 = L2
R1 = L3
L2 = Idris(6)
R0 = L2
R1 = L0
L2 = Idris(3)
R0 = L2
R1 = L1
L2 = Idris(3)
Select Case L2(0)
Case 0
R0 = L0
R1 = L1
GoTo Idris294
Case 1
Idris = Array(1)
End Select

' Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method compare
Case 288
Idris288:
L0 = R0
L1 = R1
L2 = (L0 = L1)
Select Case L2
Case CLng(&H0&)
L2 = Array(0)
Case Else
L2 = Array(1)
End Select
Select Case L2(0)
Case 0
L3 = (L0 < L1)
Select Case L3
Case CLng(&H0&)
L3 = Array(0)
Case Else
L3 = Array(1)
End Select
Select Case L3(0)
Case 0
Idris = Array(2)
Case 1
Idris = Array(0)
End Select
Case 1
Idris = Array(1)
End Select

' Prelude.VBA.Socket.Socket instance of Prelude.Show, method show
Case 317
Idris317:
L0 = R0
L1 = "Socket "
Select Case L0(0)
Case 0
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
L5 = L0(4)
L2 = L2
End Select
R0 = L2
L2 = Idris(324)
R0 = L2
L2 = Idris(98)
L3 = CInt(&H8)
L2 = Idris_LShr16(L2, L3)
L2 = CByte(L2 And &HFF)
R0 = L2
L2 = Idris(14)
Select Case L0(0)
Case 0
L3 = L0(1)
L4 = L0(2)
L5 = L0(3)
L6 = L0(4)
L3 = L3
End Select
R0 = L3
L3 = Idris(324)
R0 = L3
L3 = Idris(98)
L3 = CByte(L3 And &HFF)
R0 = L3
L3 = Idris(14)
L2 = Idris_Append(L2, L3)
Select Case L0(0)
Case 0
L3 = L0(1)
L4 = L0(2)
L5 = L0(3)
L6 = L0(4)
L3 = L3
End Select
R0 = L3
L3 = Idris(324)
L3 = Idris_UTrunc_32_16(L3)
L4 = CInt(&H8)
L3 = Idris_LShr16(L3, L4)
L3 = CByte(L3 And &HFF)
R0 = L3
L3 = Idris(14)
Select Case L0(0)
Case 0
L4 = L0(1)
L5 = L0(2)
L6 = L0(3)
L7 = L0(4)
L4 = L4
End Select
R0 = L4
L4 = Idris(324)
L4 = Idris_UTrunc_32_16(L4)
L4 = CByte(L4 And &HFF)
R0 = L4
L4 = Idris(14)
L3 = Idris_Append(L3, L4)
L2 = Idris_Append(L2, L3)
Select Case L0(0)
Case 0
L3 = L0(1)
L4 = L0(2)
L5 = L0(3)
L6 = L0(4)
L3 = L3
End Select
L3 = Idris_UTrunc_64_32(L3)
R0 = L3
L3 = Idris(98)
L4 = CInt(&H8)
L3 = Idris_LShr16(L3, L4)
L3 = CByte(L3 And &HFF)
R0 = L3
L3 = Idris(14)
Select Case L0(0)
Case 0
L4 = L0(1)
L5 = L0(2)
L6 = L0(3)
L7 = L0(4)
L4 = L4
End Select
L4 = Idris_UTrunc_64_32(L4)
R0 = L4
L4 = Idris(98)
L4 = CByte(L4 And &HFF)
R0 = L4
L4 = Idris(14)
L3 = Idris_Append(L3, L4)
Select Case L0(0)
Case 0
L4 = L0(1)
L5 = L0(2)
L6 = L0(3)
L7 = L0(4)
L4 = L4
End Select
L4 = Idris_UTrunc_64_32(L4)
L4 = Idris_UTrunc_32_16(L4)
L5 = CInt(&H8)
L4 = Idris_LShr16(L4, L5)
L4 = CByte(L4 And &HFF)
R0 = L4
L4 = Idris(14)
Select Case L0(0)
Case 0
L5 = L0(1)
L6 = L0(2)
L7 = L0(3)
L8 = L0(4)
L5 = L5
End Select
L5 = Idris_UTrunc_64_32(L5)
L5 = Idris_UTrunc_32_16(L5)
L5 = CByte(L5 And &HFF)
R0 = L5
L5 = Idris(14)
L4 = Idris_Append(L4, L5)
L3 = Idris_Append(L3, L4)
L2 = Idris_Append(L2, L3)
L3 = " "
Select Case L0(0)
Case 0
L4 = L0(1)
L5 = L0(2)
L6 = L0(3)
L7 = L0(4)
L4 = L5
End Select
Select Case L4(0)
Case 1
L4 = "AF_INET"
Case 2
L4 = "AF_INET6"
Case 0
L4 = "AF_UNSPEC"
End Select
L5 = " "
Select Case L0(0)
Case 0
L6 = L0(1)
L7 = L0(2)
L8 = L0(3)
L9 = L0(4)
L6 = L8
End Select
Select Case L6(0)
Case 2
L6 = "Datagram"
Case 0
L6 = "NotASocket"
Case 3
L6 = "Raw"
Case 1
L6 = "Stream"
End Select
L7 = " "
Select Case L0(0)
Case 0
L8 = L0(1)
L9 = L0(2)
L10 = L0(3)
L11 = L0(4)
L8 = L11
End Select
R0 = L8
L8 = Idris(98)
L9 = CInt(&H8)
L8 = Idris_LShr16(L8, L9)
L8 = CByte(L8 And &HFF)
R0 = L8
L8 = Idris(14)
Select Case L0(0)
Case 0
L9 = L0(1)
L10 = L0(2)
L11 = L0(3)
L12 = L0(4)
L9 = L12
End Select
R0 = L9
L9 = Idris(98)
L9 = CByte(L9 And &HFF)
R0 = L9
L9 = Idris(14)
L8 = Idris_Append(L8, L9)
Select Case L0(0)
Case 0
L9 = L0(1)
L10 = L0(2)
L11 = L0(3)
L12 = L0(4)
L9 = L12
End Select
L9 = Idris_UTrunc_32_16(L9)
L10 = CInt(&H8)
L9 = Idris_LShr16(L9, L10)
L9 = CByte(L9 And &HFF)
R0 = L9
L9 = Idris(14)
Select Case L0(0)
Case 0
L10 = L0(1)
L11 = L0(2)
L12 = L0(3)
L13 = L0(4)
L10 = L13
End Select
L10 = Idris_UTrunc_32_16(L10)
L10 = CByte(L10 And &HFF)
R0 = L10
L10 = Idris(14)
L9 = Idris_Append(L9, L10)
L8 = Idris_Append(L8, L9)
L7 = Idris_Append(L7, L8)
L6 = Idris_Append(L6, L7)
L5 = Idris_Append(L5, L6)
L4 = Idris_Append(L4, L5)
L3 = Idris_Append(L3, L4)
L2 = Idris_Append(L2, L3)
Idris = Idris_Append(L1, L2)

' Prelude.VBA.Socket.SocketAddress instance of Prelude.Show, method show
Case 316
Idris316:
L0 = R0
Select Case L0(0)
Case 0
L1 = L0(1)
L2 = L0(2)
L3 = L0(3)
Select Case L1(0)
Case 1
R0 = L2
L4 = Idris(35)
L5 = ":"
L6 = CInt(&H8)
L6 = Idris_LShr16(L3, L6)
L6 = CByte(L6 And &HFF)
R0 = L6
L6 = Idris(14)
L7 = CByte(L3 And &HFF)
R0 = L7
L7 = Idris(14)
L6 = Idris_Append(L6, L7)
L5 = Idris_Append(L5, L6)
Idris = Idris_Append(L4, L5)
Case Else
L4 = "SocketAddress("
Select Case L1(0)
Case 1
L5 = "AF_INET"
Case 2
L5 = "AF_INET6"
Case 0
L5 = "AF_UNSPEC"
End Select
L6 = ")"
L5 = Idris_Append(L5, L6)
Idris = Idris_Append(L4, L5)
End Select
End Select

' Prelude.VBA.Socket.SocketFamily instance of Prelude.Show, method show
Case 325
Idris325:
L0 = R0
Select Case L0(0)
Case 1
Idris = "AF_INET"
Case 2
Idris = "AF_INET6"
Case 0
Idris = "AF_UNSPEC"
End Select

' Prelude.VBA.Socket.SocketType instance of Prelude.Show, method show
Case 326
Idris326:
L0 = R0
Select Case L0(0)
Case 2
Idris = "Datagram"
Case 0
Idris = "NotASocket"
Case 3
Idris = "Raw"
Case 1
Idris = "Stream"
End Select

' Prelude.Prelude.String instance of Prelude.Show, method show
Case 318
Idris318:
L0 = R0
L1 = CInt(&H22)
R0 = L0
L2 = Idris(100)
Select Case L2(0)
Case 1
L3 = L2(1)
L4 = L2(2)
L5 = 0
R0 = L4
L6 = Idris(100)
R0 = L5
R1 = L6
L5 = Idris(327)
L2 = Array(1,L3,L5)
Case 0
L2 = Array(0)
End Select
R0 = L2
L2 = Idris(110)
L3 = """
R0 = L2
R1 = L3
L2 = Idris(3)
Idris = Idris_Append(Chr(L1), L2)

' VBA.Socket.VBA.Socket.SocketFamily instance of VBA.Socket.ToCode, method toCode
Case 62
Idris62:
L0 = R0
Select Case L0(0)
Case 1
Idris = CLng(&H2&)
Case 2
Idris = CLng(&Ha&)
Case 0
Idris = CLng(&H0&)
End Select

' VBA.Socket.VBA.Socket.SocketType instance of VBA.Socket.ToCode, method toCode
Case 308
Idris308:
L0 = R0
Select Case L0(0)
Case 2
Idris = CLng(&H2&)
Case 0
Idris = CLng(&H0&)
Case 3
Idris = CLng(&H3&)
Case 1
Idris = CLng(&H1&)
End Select

' Prelude.Bits.b32ToString, c1, upper
Case 98
Idris98:
L0 = R0
L1 = CLng(&H10&)
L1 = Idris_LShr32(L0, L1)
L2 = CLng(&Hffff&)
L1 = (L1 And L2)
Idris = Idris_UTrunc_32_16(L1)

' Prelude.Bits.b64ToString, c1, upper
Case 324
Idris324:
L0 = R0
L1 = CLngLng(&H20^)
L1 = Idris_LShr64(L0, L1)
L2 = CLngLng(&Hffffffff^)
L1 = (L1 And L2)
Idris = Idris_UTrunc_64_32(L1)

' Prelude.showLitChar, asciiTab
Case 107
Idris107:
L0 = R0
L1 = "NUL"
L2 = "SOH"
L3 = "STX"
L4 = "ETX"
L5 = "EOT"
L6 = "ENQ"
L7 = "ACK"
L8 = "BEL"
L9 = "BS"
L10 = "HT"
L11 = "LF"
L12 = "VT"
L13 = "FF"
L14 = "CR"
L15 = "SO"
L16 = "SI"
L17 = "DLE"
L18 = "DC1"
L19 = "DC2"
L20 = "DC3"
L21 = "DC4"
L22 = "NAK"
L23 = "SYN"
L24 = "ETB"
L25 = "CAN"
L26 = "EM"
L27 = "SUB"
L28 = "ESC"
L29 = "FS"
L30 = "GS"
L31 = "RS"
L32 = "US"
L33 = Array(0)
L32 = Array(1,L32,L33)
L31 = Array(1,L31,L32)
L30 = Array(1,L30,L31)
L29 = Array(1,L29,L30)
L28 = Array(1,L28,L29)
L27 = Array(1,L27,L28)
L26 = Array(1,L26,L27)
L25 = Array(1,L25,L26)
L24 = Array(1,L24,L25)
L23 = Array(1,L23,L24)
L22 = Array(1,L22,L23)
L21 = Array(1,L21,L22)
L20 = Array(1,L20,L21)
L19 = Array(1,L19,L20)
L18 = Array(1,L18,L19)
L17 = Array(1,L17,L18)
L16 = Array(1,L16,L17)
L15 = Array(1,L15,L16)
L14 = Array(1,L14,L15)
L13 = Array(1,L13,L14)
L12 = Array(1,L12,L13)
L11 = Array(1,L11,L12)
L10 = Array(1,L10,L11)
L9 = Array(1,L9,L10)
L8 = Array(1,L8,L9)
L7 = Array(1,L7,L8)
L6 = Array(1,L6,L7)
L5 = Array(1,L5,L6)
L4 = Array(1,L4,L5)
L3 = Array(1,L3,L4)
L2 = Array(1,L2,L3)
Idris = Array(1,L1,L2)

' Prelude.showLitChar, getAt
Case 108
Idris108:
L0 = R0
L1 = R1
L2 = R2
Select Case L2(0)
Case 1
L3 = L2(1)
L4 = L2(2)
Select Case L1
Case CLngLng(&H0^)
Idris = Array(1,L3)
Case Else
L5 = CLngLng(&H1^)
L5 = (L1 - L5)
L6 = 0
R0 = L6
R1 = L5
R2 = L4
GoTo Idris108
End Select
Case 0
Idris = Array(0)
End Select

' with block in Prelude.firstCharIs
Case 328
Idris328:
L0 = R0
L1 = R1
L2 = R2
Select Case L2(0)
Case 1
L3 = L2(1)
L4 = L2(2)
R0 = L0
R1 = L3
GoTo Idris3
Case 0
Idris = Array(0)
End Select

' with block in Prelude.Strings.strM
Case 329
Idris329:
L0 = R0
L1 = R1
Select Case L1(0)
Case 1
L2 = 0
L3 = 0
L4 = Array(0)
R0 = L2
R1 = L3
R2 = L4
GoTo Idris103
Case 0
L2 = 0
L3 = 0
L4 = Asc(Left(L0, 1))
L5 = Mid(L0, 2)
L4 = Array(1,L4,L5)
R0 = L2
R1 = L3
R2 = L4
GoTo Idris103
End Select

' with block in Prelude.Strings.unpack
Case 327
Idris327:
L0 = R0
L1 = R1
Select Case L1(0)
Case 1
L2 = L1(1)
L3 = L1(2)
L4 = 0
R0 = L3
L5 = Idris(100)
R0 = L4
R1 = L5
L4 = Idris(327)
Idris = Array(1,L2,L4)
Case 0
Idris = Array(0)
End Select

' with block in Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <
Case 330
Idris330:
L0 = R0
L1 = R1
L2 = R2
Select Case L0(0)
Case 0
Idris = Array(1)
Case Else
Idris = Array(0)
End Select

' with block in Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method >
Case 331
Idris331:
L0 = R0
L1 = R1
L2 = R2
Select Case L0(0)
Case 2
Idris = Array(1)
Case Else
Idris = Array(0)
End Select

' with block in Prelude.Classes.Prelude.Classes.Char instance of Prelude.Classes.Ord, method <
Case 332
Idris332:
L0 = R0
L1 = R1
L2 = R2
Select Case L0(0)
Case 0
Idris = Array(1)
Case Else
Idris = Array(0)
End Select

' with block in Prelude.Classes.Prelude.Classes.Char instance of Prelude.Classes.Ord, method >
Case 333
Idris333:
L0 = R0
L1 = R1
L2 = R2
Select Case L0(0)
Case 2
Idris = Array(1)
Case Else
Idris = Array(0)
End Select

' Prelude.Classes.Bits32 instance of Prelude.Classes.Eq
Case 161
Idris161:
L0 = R0
L1 = R1
L2 = (L0 = L1)
Select Case L2
Case CLng(&H0&)
Idris = Array(0)
Case Else
Idris = Array(1)
End Select

' Prelude.Classes.Char instance of Prelude.Classes.Ord
Case 109
Idris109:
L0 = Array(65676)
L1 = Array(65678)
L2 = Array(65680)
Idris = Array(0,L0,L1,L2)

' Prelude.Classes.Int instance of Prelude.Classes.Ord
Case 314
Idris314:
L0 = Array(65682)
L1 = Array(65684)
L2 = Array(65686)
Idris = Array(0,L0,L1,L2)

' case block in Void
Case 334
Idris334:
Idris = 0

' VBA.Socket.case block in accept
Case 335
Idris335:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = R6
L7 = R7
L8 = R8
L9 = R9
Select Case L6(0)
Case 1
L10 = L6(1)
L11 = 0
L12 = 0
Select Case L7(0)
Case 0
L13 = L7(1)
L14 = L7(2)
L13 = L13
End Select
R0 = L11
R1 = L12
R2 = L13
L11 = Idris(102)
Select Case L10(0)
Case 0
L12 = L10(1)
L13 = L10(2)
L14 = L10(3)
L12 = L12
End Select
Select Case L0(0)
Case 0
L13 = L0(1)
L14 = L0(2)
L15 = L0(3)
L16 = L0(4)
L13 = L15
End Select
Select Case L0(0)
Case 0
L14 = L0(1)
L15 = L0(2)
L16 = L0(3)
L17 = L0(4)
L14 = L17
End Select
L12 = Array(0,L4,L12,L13,L14)
L12 = Array(0,L12,L10)
L12 = Array(1,L12)
L12 = Array(1,L12)
R0 = L11
R1 = L12
GoTo Idris3
Case 0
L10 = 0
L11 = 0
L12 = 0
R0 = L10
R1 = L11
R2 = L12
R3 = L7
L10 = Idris(7)
L11 = Array(65724,L4)
R0 = L10
R1 = L11
L10 = Idris(3)
L11 = Array(65760,L7)
R0 = L10
R1 = L11
GoTo Idris3
End Select

' case block in io_bind
Case 336
Idris336:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = R6
L7 = R7
R0 = L7
R1 = L5
GoTo Idris3

' Main.case block in main
Case 337
Idris337:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = R6
L7 = R7
L8 = R8
L9 = R9
L10 = R10
L11 = R11
L12 = R12
L13 = R13
L14 = R14
L15 = R15
L16 = R16
L17 = R17
L18 = R18
L19 = R19
L20 = R20
L21 = R21
L22 = R22
L23 = R23
L24 = R24
L25 = R25
L26 = R26
L27 = R27
L28 = R28
L29 = R29
L30 = R30
L31 = R31
L32 = R32
L33 = R33
L34 = R34
L35 = R35
L36 = R36
L37 = R37
L38 = R38
Select Case L35(0)
Case 1
L39 = L35(1)
Select Case L39(0)
Case 0
L40 = L39(1)
L41 = L39(2)
L42 = 0
L43 = 0
L44 = 0
R0 = L42
R1 = L43
R2 = L44
R3 = L37
L42 = Idris(7)
L43 = 0
L44 = 0
L45 = 0
L46 = "Accepted client "
R0 = L41
L47 = Idris(316)
L46 = Idris_Append(L46, L47)
L47 = ChrW(10)
L46 = Idris_Append(L46, L47)
R0 = L45
R1 = L46
L45 = Idris(47)
R0 = L43
R1 = L44
R2 = L45
L43 = Idris(119)
R0 = L42
R1 = L43
L42 = Idris(3)
L43 = Array(65633,L37,L40)
R0 = L42
R1 = L43
GoTo Idris3
End Select
Case 0
L39 = 0
L40 = 0
L41 = 0
L42 = "Did not accept client" & ChrW(10)
R0 = L41
R1 = L42
L41 = Idris(47)
R0 = L39
R1 = L40
R2 = L41
GoTo Idris119
End Select

' VBA.Socket.case block in peekSockAddr
Case 338
Idris338:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = 0
L5 = 0
L6 = 0
R0 = L4
R1 = L5
R2 = L6
R3 = L1
L4 = Idris(7)
L5 = CLngLng(&H2^)
R0 = L0
R1 = L5
L5 = Idris(57)
L5 = Array(65716,L5)
R0 = L4
R1 = L5
L4 = Idris(3)
L5 = Array(65763,L1,L0,L2)
R0 = L4
R1 = L5
GoTo Idris3

' Prelude.case block in showLitChar
Case 339
Idris339:
L0 = R0
L1 = R1
Select Case L1(0)
Case 1
L2 = L1(1)
L3 = 0
L4 = 0
L5 = 0
L6 = CInt(&H5c)
L6 = Array(65813,L6)
L7 = Array(65696,L2)
Idris = Array(65665,L3,L4,L5,L6,L7)
Case 0
L2 = 0
L3 = Idris(109)
R0 = L2
R1 = L3
L2 = Idris(23)
R0 = L2
R1 = L0
L2 = Idris(3)
L3 = CInt(&H7f)
R0 = L2
R1 = L3
L2 = Idris(3)
Select Case L2(0)
Case 2
L2 = Array(1)
Case Else
L2 = Array(0)
End Select
Select Case L2(0)
Case 0
Idris = Array(65813,L0)
Case 1
L3 = 0
L4 = 0
L5 = 0
L6 = CInt(&H5c)
L6 = Array(65813,L6)
L7 = Array(65673)
L8 = CLng(L0)
L8 = CStr(L8)
L7 = Array(65695,L7,L8)
Idris = Array(65665,L3,L4,L5,L6,L7)
End Select
End Select

' VBA.Except.case block in ve_run
Case 340
Idris340:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = R6
L7 = R7
L8 = R8
Select Case L7(0)
Case 0
L9 = L7(1)
R0 = L4
R1 = L9
GoTo Idris3
Case 1
L9 = L7(1)
R0 = L5
R1 = L9
GoTo Idris3
End Select

' Prelude.Monad.case block in VBA.Except.VBAExcept e instance of Prelude.Monad.Monad, method >>=
Case 341
Idris341:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = R6
L7 = R7
Select Case L6(0)
Case 0
L8 = L6(1)
L9 = 0
L10 = 0
Select Case L5(0)
Case 0
L11 = L5(1)
L12 = L5(2)
L11 = L11
End Select
R0 = L9
R1 = L10
R2 = L11
L9 = Idris(102)
L10 = Array(0,L8)
R0 = L9
R1 = L10
GoTo Idris3
Case 1
L8 = L6(1)
R0 = L4
R1 = L8
GoTo Idris3
End Select

' VBA.Socket.case block in case block in peekSockAddr
Case 342
Idris342:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = R6
L7 = 0
L8 = 0
L9 = 0
R0 = L7
R1 = L8
R2 = L9
R3 = L4
L7 = Idris(7)
L8 = CLngLng(&H4^)
R0 = L0
R1 = L8
L8 = Idris(57)
L8 = Array(65717,L8)
R0 = L7
R1 = L8
L7 = Idris(3)
L8 = Array(65761,L2,L4,L5)
R0 = L7
R1 = L8
GoTo Idris3

' Prelude.Monad.case block in case block in VBA.Except.VBAExcept e instance of Prelude.Monad.Monad, method >>=
Case 343
Idris343:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = R6
L7 = R7
L8 = R8
Idris = L8

' VBA.Socket.case block in case block in case block in peekSockAddr
Case 344
Idris344:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = R6
L7 = R7
L8 = R8
L9 = R9
L10 = CLng(L2 And &HFF^)
R0 = L10
L10 = Idris(28)
Select Case L10(0)
Case 1
L11 = L10(1)
L12 = 0
L13 = 0
Select Case L7(0)
Case 0
L14 = L7(1)
L15 = L7(2)
L14 = L14
End Select
R0 = L12
R1 = L13
R2 = L14
L12 = Idris(102)
R0 = L8
L13 = Idris(50)
R0 = L5
L14 = Idris(51)
L13 = Array(0,L11,L13,L14)
L13 = Array(1,L13)
R0 = L12
R1 = L13
GoTo Idris3
Case 0
L11 = 0
L12 = 0
Select Case L7(0)
Case 0
L13 = L7(1)
L14 = L7(2)
L13 = L13
End Select
R0 = L11
R1 = L12
R2 = L13
L11 = Idris(102)
L12 = Array(0)
R0 = L11
R1 = L12
GoTo Idris3
End Select

' VBA.Socket.case block in case block in case block in case block in peekSockAddr
Case 345
Idris345:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = R6
L7 = R7
L8 = R8
L9 = R9
L10 = R10
Select Case L10(0)
Case 1
L11 = L10(1)
L12 = 0
L13 = 0
Select Case L7(0)
Case 0
L14 = L7(1)
L15 = L7(2)
L14 = L14
End Select
R0 = L12
R1 = L13
R2 = L14
L12 = Idris(102)
R0 = L8
L13 = Idris(50)
R0 = L5
L14 = Idris(51)
L13 = Array(0,L11,L13,L14)
L13 = Array(1,L13)
R0 = L12
R1 = L13
GoTo Idris3
Case 0
L11 = 0
L12 = 0
Select Case L7(0)
Case 0
L13 = L7(1)
L14 = L7(2)
L13 = L13
End Select
R0 = L11
R1 = L12
R2 = L13
L11 = Idris(102)
L12 = Array(0)
R0 = L11
R1 = L12
GoTo Idris3
End Select

' <<Void eliminator>>
Case 346
Idris346:
Idris = 0

End Select
End Function

' Main Entry Point
Public Sub Main
    Call Idris_InitRuntime
    Call Idris(307)
End Sub
