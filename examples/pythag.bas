' Foreign Functions


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

' Prelude.Applicative.*>
Case 0
Idris0:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = 0
L7 = 0
L8 = 0
R0 = L6
R1 = L7
R2 = L8
R3 = L3
L6 = Idris(1)
L7 = 0
L8 = 0
L9 = 0
Select Case L3(0)
Case 0
L10 = L3(1)
L11 = L3(2)
L12 = L3(3)
L10 = L10
End Select
R0 = L7
R1 = L8
R2 = L9
R3 = L10
L7 = Idris(2)
L8 = 0
L9 = 0
L10 = 0
L10 = Array(65684,L10)
L8 = Array(65683,L8,L9,L10)
R0 = L7
R1 = L8
L7 = Idris(3)
R0 = L7
R1 = L4
L7 = Idris(3)
R0 = L6
R1 = L7
L6 = Idris(3)
R0 = L6
R1 = L5
GoTo Idris3

' Prelude.List.++
Case 4
Idris4:
L0 = R0
L1 = R1
L2 = R2
Select Case L1(0)
Case 1
L3 = L1(1)
L4 = L1(2)
L5 = 0
R0 = L5
R1 = L4
R2 = L2
L5 = Idris(4)
Idris = Array(1,L3,L5)
Case 0
Idris = L2
End Select

' Prelude.Basics..
Case 5
Idris5:
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
Case 6
Idris6:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
Idris = L3
End Select

' Prelude.Applicative.<*>
Case 1
Idris1:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
Select Case L3(0)
Case 0
L4 = L3(1)
L5 = L3(2)
L6 = L3(3)
R0 = L6
R1 = L1
L7 = Idris(3)
R0 = L7
R1 = L2
GoTo Idris3
End Select

' Prelude.Algebra.<+>
Case 7
Idris7:
L0 = R0
L1 = R1
Idris = L1

' @@constructor of Prelude.Algebra.Monoid#Semigroup a
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

' @@constructor of Prelude.Applicative.Alternative#Applicative f
Case 9
Idris9:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
Idris = L2
End Select

' @@constructor of Prelude.Applicative.Applicative#Functor f
Case 10
Idris10:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
L4 = L1(3)
Idris = L2
End Select

' Force
Case 11
Idris11:
L0 = R0
L1 = R1
L2 = R2
R0 = L2
L3 = Idris(12)
Idris = L3

' Prelude.Bool.boolElim
Case 13
Idris13:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
Select Case L1(0)
Case 0
R0 = L3
GoTo Idris12
Case 1
R0 = L2
GoTo Idris12
End Select

' call__IO
Case 14
Idris14:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
R0 = L2
R1 = L3
GoTo Idris3

' VBA.Excel.clearCells
Case 15
Idris15:
L0 = R0
Call Cells.DeleteIdris = 0

' Prelude.Classes.compare
Case 16
Idris16:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
Idris = L2
End Select

' Prelude.Foldable.concatMap
Case 17
Idris17:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = 0
L7 = 0
L8 = 0
R0 = L6
R1 = L7
R2 = L8
R3 = L3
L6 = Idris(18)
L7 = 0
L8 = 0
L9 = 0
Select Case L4(0)
Case 0
L10 = L4(1)
L11 = L4(2)
L10 = L10
End Select
L7 = Array(65682,L7,L8,L9,L10,L5)
R0 = L6
R1 = L7
L6 = Idris(3)
Select Case L4(0)
Case 0
L7 = L4(1)
L8 = L4(2)
L7 = L8
End Select
R0 = L6
R1 = L7
GoTo Idris3

' Prelude.Basics.const
Case 19
Idris19:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
Idris = L2

' Prelude.Applicative.empty
Case 20
Idris20:
L0 = R0
L1 = R1
L2 = R2
Select Case L2(0)
Case 0
L3 = L2(1)
L4 = L2(2)
R0 = L4
R1 = L1
GoTo Idris3
End Select

' Prelude.Foldable.foldr
Case 18
Idris18:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
R0 = L3
R1 = L1
L4 = Idris(3)
R0 = L4
R1 = L2
GoTo Idris3

' Prelude.List.foldrImpl
Case 21
Idris21:
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
L10 = Array(65682,L10,L11,L12,L4,L13)
R0 = L8
R1 = L9
R2 = L2
R3 = L3
R4 = L10
R5 = L7
GoTo Idris21
Case 0
R0 = L4
R1 = L3
GoTo Idris3
End Select

' Prelude.Applicative.guard
Case 22
Idris22:
L0 = R0
L1 = R1
L2 = R2
Select Case L2(0)
Case 0
Select Case L1(0)
Case 0
L3 = L1(1)
L4 = L1(2)
L5 = 0
R0 = L4
R1 = L5
GoTo Idris3
End Select
Case 1
L3 = 0
L4 = 0
Select Case L1(0)
Case 0
L5 = L1(1)
L6 = L1(2)
L5 = L5
End Select
R0 = L3
R1 = L4
R2 = L5
L3 = Idris(23)
L4 = Array(0)
R0 = L3
R1 = L4
GoTo Idris3
End Select

' Prelude.Basics.id
Case 24
Idris24:
L0 = R0
L1 = R1
Idris = L1

' Prelude.Classes.intToBool
Case 25
Idris25:
L0 = R0
Select Case L0
Case CLng(&H0&)
Idris = Array(0)
Case Else
Idris = Array(1)
End Select

' io_bind
Case 26
Idris26:
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
L6 = Idris(27)
R0 = L3
R1 = L5
L7 = Idris(3)
R0 = L6
R1 = L7
GoTo Idris3

' io_return
Case 28
Idris28:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
Idris = L2

' Main.main
Case 29
Idris29:
L0 = 0
L1 = 0
L2 = 0
L3 = 0
L4 = "Clearing cells..." & ChrW(10)
R0 = L3
R1 = L4
L3 = Idris(30)
L4 = Array(65645)
Idris = Array(65702,L0,L1,L2,L3,L4)

' Prelude.Functor.map
Case 2
Idris2:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
R0 = L3
R1 = L1
L4 = Idris(3)
R0 = L4
R1 = L2
GoTo Idris3

' mkForeignPrim
Case 31
Idris31:
Idris = 0

' Prelude.Algebra.neutral
Case 32
Idris32:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
Idris = L3
End Select

' prim__addInt
Case 33
Idris33:
L0 = R0
L1 = R1
Idris = (L0 + L1)

' prim__eqInt
Case 34
Idris34:
L0 = R0
L1 = R1
Idris = (L0 = L1)

' prim__mulInt
Case 35
Idris35:
L0 = R0
L1 = R1
Idris = (L0 * L1)

' prim__sextInt_BigInt
Case 36
Idris36:
L0 = R0
Idris = CLngLng(L0)

' prim__sltInt
Case 37
Idris37:
L0 = R0
L1 = R1
Idris = (L0 < L1)

' prim__subInt
Case 38
Idris38:
L0 = R0
L1 = R1
Idris = (L0 - L1)

' prim__toStrInt
Case 39
Idris39:
L0 = R0
Idris = CStr(L0)

' prim__writeString
Case 40
Idris40:
L0 = R0
L1 = R1
Idris = Idris_WriteStr(L1)

' prim_io_bind
Case 41
Idris41:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
R0 = L3
R1 = L2
GoTo Idris3

' Prelude.Applicative.pure
Case 23
Idris23:
L0 = R0
L1 = R1
L2 = R2
Select Case L2(0)
Case 0
L3 = L2(1)
L4 = L2(2)
L5 = L2(3)
R0 = L4
R1 = L1
GoTo Idris3
End Select

' VBA.Excel.putCell
Case 42
Idris42:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
Cells(L0,L1)=L2
Idris = 0

' Main.putResult
Case 43
Idris43:
L0 = R0
Select Case L0(0)
Case 0
L1 = L0(1)
L2 = L0(2)
Select Case L2(0)
Case 0
L3 = L2(1)
L4 = L2(2)
Select Case L4(0)
Case 0
L5 = L4(1)
L6 = L4(2)
L7 = 0
L8 = 0
L9 = 0
L10 = CLng(&H1&)
L11 = CStr(L3)
L10 = Array(65701,L1,L10,L11)
L11 = Array(65655,L1,L5,L6)
Idris = Array(65702,L7,L8,L9,L10,L11)
End Select
End Select
End Select

' Prelude.putStr
Case 30
Idris30:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = Array(65698,L1)
L6 = Array(65699)
Idris = Array(65702,L2,L3,L4,L5,L6)

' Main.pythag
Case 44
Idris44:
L0 = R0
L1 = 0
L2 = 0
L3 = CLng(&H1&)
R0 = L3
R1 = L0
L3 = Idris(45)
L4 = Array(65661)
R0 = L1
R1 = L2
R2 = L3
R3 = L4
GoTo Idris46

' run__IO
Case 47
Idris47:
L0 = R0
L1 = R1
L2 = 0
R0 = L1
R1 = L2
GoTo Idris3

' Prelude.Traversable.traverse_
Case 48
Idris48:
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
L7 = Idris(18)
L8 = 0
L9 = 0
L10 = 0
L11 = 0
L12 = 0
L13 = 0
L11 = Array(65705,L11,L12,L13,L5)
L8 = Array(65682,L8,L9,L10,L11,L6)
R0 = L7
R1 = L8
L7 = Idris(3)
L8 = 0
L9 = 0
R0 = L8
R1 = L9
R2 = L5
L8 = Idris(23)
L9 = Array(0)
R0 = L8
R1 = L9
L8 = Idris(3)
R0 = L7
R1 = L8
GoTo Idris3

' unsafePerformPrimIO
Case 49
Idris49:
Idris = 0

' world
Case 50
Idris50:
L0 = R0
Idris = L0

' Main.zipIx
Case 51
Idris51:
L0 = R0
L1 = R1
L2 = R2
Select Case L2(0)
Case 1
L3 = L2(1)
L4 = L2(2)
L5 = Array(0,L1,L3)
L6 = 0
L7 = CLng(&H1&)
L7 = (L1 + L7)
R0 = L6
R1 = L7
R2 = L4
L6 = Idris(51)
Idris = Array(1,L5,L6)
Case 0
Idris = Array(0)
End Select

' Prelude.Bool.||
Case 52
Idris52:
L0 = R0
L1 = R1
Select Case L0(0)
Case 0
R0 = L1
GoTo Idris12
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
GoTo Idris43
Case 65632
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris53
Case 65633
R0 = L1
GoTo Idris54
Case 65634
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris55
Case 65635
R0 = L1
GoTo Idris56
Case 65636
R0 = L1
GoTo Idris57
Case 65637
R0 = L1
GoTo Idris58
Case 65638
R0 = L1
GoTo Idris59
Case 65639
R0 = L1
GoTo Idris60
Case 65640
R0 = L1
GoTo Idris61
Case 65641
R0 = L1
GoTo Idris62
Case 65642
R0 = L1
GoTo Idris63
Case 65643
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris64
Case 65644
R0 = L1
GoTo Idris65
Case 65645
R0 = L1
GoTo Idris66
Case 65646
R0 = L1
GoTo Idris67
Case 65647
R0 = L1
GoTo Idris68
Case 65648
R0 = L1
GoTo Idris69
Case 65649
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris70
Case 65650
R0 = L1
GoTo Idris71
Case 65651
R0 = L1
GoTo Idris72
Case 65652
R0 = L1
GoTo Idris73
Case 65653
R0 = L1
GoTo Idris74
Case 65654
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris75
Case 65655
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris76
Case 65656
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris77
Case 65657
R0 = L1
GoTo Idris78
Case 65658
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris79
Case 65659
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris80
Case 65660
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris81
Case 65661
R0 = L1
GoTo Idris82
Case 65662
R0 = L1
GoTo Idris83
Case 65663
R0 = L1
GoTo Idris84
Case 65664
R0 = L1
GoTo Idris85
Case 65665
R0 = L1
GoTo Idris86
Case 65666
R0 = L1
GoTo Idris87
Case 65667
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris88
Case 65668
R0 = L1
GoTo Idris89
Case 65669
R0 = L1
GoTo Idris90
Case 65670
R0 = L1
GoTo Idris91
Case 65671
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
GoTo Idris0
Case 65672
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris92
Case 65673
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris93
Case 65674
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris94
Case 65675
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris95
Case 65676
R0 = L1
GoTo Idris96
Case 65677
R0 = L1
GoTo Idris97
Case 65678
R0 = L1
GoTo Idris98
Case 65679
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris99
Case 65680
R0 = L1
GoTo Idris100
Case 65681
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris101
Case 65682
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
GoTo Idris5
Case 65683
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris19
Case 65684
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris24
Case 65685
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris102
Case 65686
R0 = L1
GoTo Idris103
Case 65687
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris104
Case 65688
R0 = L1
GoTo Idris105
Case 65689
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris106
Case 65690
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris107
Case 65691
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris108
Case 65692
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris109
Case 65693
R0 = L1
GoTo Idris110
Case 65694
R0 = L1
GoTo Idris111
Case 65695
R0 = L1
GoTo Idris112
Case 65696
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris113
Case 65697
R0 = L1
GoTo Idris114
Case 65698
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris115
Case 65699
R0 = L1
GoTo Idris116
Case 65700
R0 = L1
GoTo Idris15
Case 65701
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris42
Case 65702
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
GoTo Idris26
Case 65703
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris28
Case 65704
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
GoTo Idris117
Case 65705
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
L5 = L0(4)
Idris = Array(65671,L2,L3,L4,L5,L1)
Case 65706
L2 = L0(1)
L3 = L0(2)
Idris = Array(65690,L2,L3,L1)
Case 65707
L2 = L0(1)
Idris = Array(65706,L2,L1)
Case 65708
Idris = Array(65707,L1)
Case Else
Idris = 0
End Select

' {EVAL0}
Case 12
Idris12:
L0 = R0
Select Case L0
Case Else
Idris = L0
End Select

' Prelude.Classes.{Int instance of Prelude.Classes.Ord0}
Case 102
Idris102:
L0 = R0
L1 = R1
R0 = L0
R1 = L1
GoTo Idris118

' Prelude.Classes.{Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=0}
Case 119
Idris119:
L0 = R0
L1 = R1
L2 = (L0 = L1)
Select Case L2
Case CLng(&H0&)
Idris = Array(0)
Case Else
Idris = Array(1)
End Select

' Prelude.Applicative.{Prelude.IO' ffi instance of Prelude.Applicative.Applicative, method <*>0}
Case 92
Idris92:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
R0 = L0
R1 = L1
L4 = Idris(3)
Idris = Array(65703,L2,L3,L4)

' Prelude.Functor.{Prelude.IO' ffi instance of Prelude.Functor.Functor, method map0}
Case 106
Idris106:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
R0 = L0
R1 = L1
L4 = Idris(3)
Idris = Array(65703,L2,L3,L4)

' Prelude.Applicative.{Prelude.List instance of Prelude.Applicative.Applicative, method <*>0}
Case 94
Idris94:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
L5 = Array(65684,L5)
R0 = L3
R1 = L4
R2 = L0
R3 = L1
R4 = L5
R5 = L2
GoTo Idris21

' Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=0}
Case 108
Idris108:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
L5 = Array(65684,L5)
R0 = L3
R1 = L4
R2 = L0
R3 = L1
R4 = L5
R5 = L2
GoTo Idris21

' {io_bind0}
Case 120
Idris120:
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

' Main.{main0}
Case 53
Idris53:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
L5 = Array(65684,L5)
R0 = L3
R1 = L4
R2 = L0
R3 = L1
R4 = L5
R5 = L2
GoTo Idris21

' Main.{putResult0}
Case 75
Idris75:
L0 = R0
L1 = R1
L2 = R2
L3 = CLng(&H3&)
L4 = CStr(L1)
Idris = Array(65701,L0,L3,L4)

' Prelude.{putStr0}
Case 115
Idris115:
L0 = R0
L1 = R1
Idris = Idris_WriteStr(L0)

' Main.{pythag0}
Case 77
Idris77:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
R0 = L2
R1 = L3
R2 = L0
R3 = L1
GoTo Idris121

' {runMain0}
Case 122
Idris122:
L0 = Idris(29)
L1 = 0
R0 = L0
R1 = L1
L0 = Idris(3)
R0 = L0
GoTo Idris12

' Prelude.Classes.{Int instance of Prelude.Classes.Ord1}
Case 103
Idris103:
L0 = R0
Idris = Array(65685,L0)

' Prelude.Applicative.{Prelude.IO' ffi instance of Prelude.Applicative.Applicative, method <*>1}
Case 93
Idris93:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = Array(65672,L1)
Idris = Array(65702,L2,L3,L4,L0,L5)

' Prelude.Applicative.{Prelude.List instance of Prelude.Applicative.Applicative, method <*>1}
Case 95
Idris95:
L0 = R0
L1 = R1
Idris = Array(65674,L0,L1)

' Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=1}
Case 109
Idris109:
L0 = R0
L1 = R1
Idris = Array(65691,L0,L1)

' {io_bind1}
Case 117
Idris117:
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
L7 = Idris(120)
R0 = L7
R1 = L5
GoTo Idris3

' Main.{main1}
Case 64
Idris64:
L0 = R0
L1 = R1
Idris = Array(65632,L0,L1)

' Main.{putResult1}
Case 76
Idris76:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = 0
L5 = 0
L6 = 0
L7 = CLng(&H2&)
L8 = CStr(L1)
L7 = Array(65701,L0,L7,L8)
L8 = Array(65654,L0,L2)
Idris = Array(65702,L4,L5,L6,L7,L8)

' Prelude.{putStr1}
Case 116
Idris116:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(0)
Idris = Array(65703,L1,L2,L3)

' Main.{pythag1}
Case 83
Idris83:
L0 = R0
Idris = Array(65656,L0)

' Prelude.Classes.{Int instance of Prelude.Classes.Ord2}
Case 104
Idris104:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(123)
R0 = L2
R1 = L3
L2 = Idris(16)
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

' Prelude.Applicative.{Prelude.List instance of Prelude.Applicative.Applicative, method <*>2}
Case 96
Idris96:
L0 = R0
Idris = Array(65675,L0)

' Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=2}
Case 110
Idris110:
L0 = R0
Idris = Array(65692,L0)

' {io_bind2}
Case 27
Idris27:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
Idris = Array(65704,L0,L1,L2,L3,L4,L5)

' Main.{main2}
Case 67
Idris67:
L0 = R0
Idris = Array(65643,L0)

' Main.{pythag2}
Case 84
Idris84:
L0 = R0
Idris = Array(65662)

' Prelude.Classes.{Int instance of Prelude.Classes.Ord3}
Case 105
Idris105:
L0 = R0
Idris = Array(65687,L0)

' Prelude.Applicative.{Prelude.List instance of Prelude.Applicative.Applicative, method <*>3}
Case 97
Idris97:
L0 = R0
Idris = Array(65676)

' Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=3}
Case 111
Idris111:
L0 = R0
Idris = Array(65693)

' Main.{main3}
Case 68
Idris68:
L0 = R0
Idris = Array(65646)

' Main.{pythag3}
Case 85
Idris85:
L0 = R0
Idris = Array(65663)

' Prelude.Applicative.{Prelude.List instance of Prelude.Applicative.Applicative, method <*>4}
Case 98
Idris98:
L0 = R0
Idris = Array(65677)

' Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=4}
Case 112
Idris112:
L0 = R0
Idris = Array(65694)

' Main.{main4}
Case 69
Idris69:
L0 = R0
Idris = Array(65647)

' Main.{pythag4}
Case 86
Idris86:
L0 = R0
L1 = Array(0)
Idris = Array(1,L0,L1)

' Prelude.Applicative.{Prelude.List instance of Prelude.Applicative.Applicative, method <*>5}
Case 99
Idris99:
L0 = R0
L1 = R1
L2 = 0
R0 = L2
R1 = L0
R2 = L1
GoTo Idris4

' Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=5}
Case 113
Idris113:
L0 = R0
L1 = R1
L2 = 0
R0 = L2
R1 = L0
R2 = L1
GoTo Idris4

' Main.{main5}
Case 70
Idris70:
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
GoTo Idris124

' Main.{pythag5}
Case 87
Idris87:
L0 = R0
Idris = Array(65665)

' Prelude.Applicative.{Prelude.List instance of Prelude.Applicative.Applicative, method <*>6}
Case 100
Idris100:
L0 = R0
Idris = Array(65679,L0)

' Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=6}
Case 114
Idris114:
L0 = R0
Idris = Array(65696,L0)

' Main.{main6}
Case 71
Idris71:
L0 = R0
Idris = Array(65649,L0)

' Main.{pythag6}
Case 88
Idris88:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
R0 = L2
R1 = L3
R2 = L0
R3 = L1
GoTo Idris125

' Prelude.Applicative.{Prelude.List instance of Prelude.Applicative.Applicative, method <*>7}
Case 101
Idris101:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
R0 = L2
R1 = L3
R2 = L1
R3 = L0
GoTo Idris121

' Main.{main7}
Case 72
Idris72:
L0 = R0
Idris = Array(65650)

' Main.{pythag7}
Case 89
Idris89:
L0 = R0
Idris = Array(65667,L0)

' Main.{main8}
Case 73
Idris73:
L0 = R0
Idris = Array(65651)

' Main.{pythag8}
Case 90
Idris90:
L0 = R0
Idris = Array(65668)

' Main.{main9}
Case 74
Idris74:
L0 = R0
L1 = 0
L2 = 0
Idris = Array(65703,L1,L2,L0)

' Main.{pythag9}
Case 91
Idris91:
L0 = R0
Idris = Array(65669)

' Main.{main10}
Case 54
Idris54:
L0 = R0
Idris = Array(65653)

' Main.{pythag10}
Case 78
Idris78:
L0 = R0
Idris = Array(0)

' Main.{main11}
Case 55
Idris55:
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
GoTo Idris126

' Main.{pythag11}
Case 79
Idris79:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = Array(0,L1,L2)
L4 = Array(0,L0,L4)
L5 = Array(0)
Idris = Array(1,L4,L5)

' Main.{main12}
Case 56
Idris56:
L0 = R0
Idris = Array(65634,L0)

' Main.{pythag12}
Case 80
Idris80:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
L6 = Array(65664)
L7 = Array(65666)
L8 = Array(65670)
L6 = Array(0,L6,L7,L8)
L7 = Array(65657)
L6 = Array(0,L6,L7)
L7 = (L2 * L2)
L8 = (L0 * L0)
L7 = (L7 + L8)
L8 = (L1 * L1)
L7 = (L7 = L8)
Select Case L7
Case CLng(&H0&)
L7 = Array(0)
Case Else
L7 = Array(1)
End Select
R0 = L5
R1 = L6
R2 = L7
L5 = Idris(22)
L6 = Array(65658,L2,L0,L1)
R0 = L3
R1 = L4
R2 = L5
R3 = L6
GoTo Idris46

' Main.{main13}
Case 57
Idris57:
L0 = R0
Idris = Array(65635)

' Main.{pythag13}
Case 81
Idris81:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = CLng(&H1&)
R0 = L4
R1 = L1
L4 = Idris(45)
L5 = Array(65659,L1,L0)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
GoTo Idris46

' Main.{main14}
Case 58
Idris58:
L0 = R0
Idris = Array(65636)

' Main.{pythag14}
Case 82
Idris82:
L0 = R0
L1 = 0
L2 = 0
L3 = CLng(&H1&)
R0 = L3
R1 = L0
L3 = Idris(45)
L4 = Array(65660,L0)
R0 = L1
R1 = L2
R2 = L3
R3 = L4
GoTo Idris46

' Main.{main15}
Case 59
Idris59:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = 0
L5 = Array(65648)
L6 = Array(65652)
L7 = Array(65633)
L8 = Array(65637)
L6 = Array(0,L6,L7,L8)
L7 = Array(65631)
R0 = L1
R1 = L2
R2 = L3
R3 = L4
R4 = L5
R5 = L6
R6 = L7
L1 = Idris(48)
L2 = 0
L3 = CLng(&H2&)
L4 = CLng(&H32&)
R0 = L4
L4 = Idris(44)
R0 = L2
R1 = L3
R2 = L4
L2 = Idris(51)
R0 = L1
R1 = L2
GoTo Idris3

' Main.{main16}
Case 60
Idris60:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = 0
L5 = "Calculating..." & ChrW(10)
R0 = L4
R1 = L5
L4 = Idris(30)
L5 = Array(65638)
Idris = Array(65702,L1,L2,L3,L4,L5)

' Main.{main17}
Case 61
Idris61:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = CLng(&H1&)
L5 = CLng(&H3&)
L6 = "Z"
L4 = Array(65701,L4,L5,L6)
L5 = Array(65639)
Idris = Array(65702,L1,L2,L3,L4,L5)

' Main.{main18}
Case 62
Idris62:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = CLng(&H1&)
L5 = CLng(&H2&)
L6 = "Y"
L4 = Array(65701,L4,L5,L6)
L5 = Array(65640)
Idris = Array(65702,L1,L2,L3,L4,L5)

' Main.{main19}
Case 63
Idris63:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = CLng(&H1&)
L5 = CLng(&H1&)
L6 = "X"
L4 = Array(65701,L4,L5,L6)
L5 = Array(65641)
Idris = Array(65702,L1,L2,L3,L4,L5)

' Main.{main20}
Case 65
Idris65:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = 0
L5 = "Writing headers..." & ChrW(10)
R0 = L4
R1 = L5
L4 = Idris(30)
L5 = Array(65642)
Idris = Array(65702,L1,L2,L3,L4,L5)

' Main.{main21}
Case 66
Idris66:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = Array(65700)
L5 = Array(65644)
Idris = Array(65702,L1,L2,L3,L4,L5)

' Decidable.Equality.Decidable.Equality.Char instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 127
Idris127:
Idris = 0

' Decidable.Equality.Decidable.Equality.Int instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 128
Idris128:
Idris = 0

' Decidable.Equality.Decidable.Equality.Integer instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 129
Idris129:
Idris = 0

' Decidable.Equality.Decidable.Equality.String instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 130
Idris130:
Idris = 0

' Prelude.Prelude.Int instance of Prelude.Enum, method enumFromTo, go
Case 131
Idris131:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
Select Case L3
Case CLngLng(&H0^)
Idris = Array(1,L4,L2)
Case Else
L5 = CLngLng(&H1^)
L5 = (L3 - L5)
L6 = 0
L7 = 0
L8 = Array(1,L4,L2)
L9 = CLng(&H1&)
L9 = (L4 - L9)
R0 = L6
R1 = L7
R2 = L8
R3 = L5
R4 = L9
GoTo Idris131
End Select

' Prelude.Applicative.Prelude.IO' ffi instance of Prelude.Applicative.Applicative, method <*>
Case 126
Idris126:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = 0
L6 = 0
L7 = 0
L8 = Array(65673,L4)
Idris = Array(65702,L5,L6,L7,L3,L8)

' Prelude.Applicative.Prelude.List instance of Prelude.Applicative.Applicative, method <*>
Case 125
Idris125:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = 0
L5 = 0
L6 = 0
L7 = Array(65678)
L8 = Array(65680)
L9 = Array(0)
L8 = Array(0,L8,L9)
L9 = Array(65681,L3)
R0 = L4
R1 = L5
R2 = L6
R3 = L7
R4 = L8
R5 = L9
L4 = Idris(17)
R0 = L4
R1 = L2
GoTo Idris3

' Prelude.Prelude.Int instance of Prelude.Enum, method enumFromTo
Case 45
Idris45:
L0 = R0
L1 = R1
R0 = L0
R1 = L1
L2 = Idris(132)
Select Case L2(0)
Case 0
Idris = Array(0)
Case 1
L3 = 0
L4 = 0
L5 = Array(0)
L6 = (L1 - L0)
L6 = CLngLng(L6)
R0 = L3
R1 = L4
R2 = L5
R3 = L6
R4 = L1
GoTo Idris131
End Select

' Prelude.Functor.Prelude.IO' ffi instance of Prelude.Functor.Functor, method map
Case 124
Idris124:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = 0
L6 = 0
L7 = 0
L8 = Array(65689,L3)
Idris = Array(65702,L5,L6,L7,L4,L8)

' Prelude.Functor.Prelude.List.List instance of Prelude.Functor.Functor, method map
Case 121
Idris121:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
Select Case L3(0)
Case 1
L4 = L3(1)
L5 = L3(2)
R0 = L2
R1 = L4
L6 = Idris(3)
L7 = 0
L8 = 0
L9 = 0
L10 = Array(65708)
R0 = L7
R1 = L8
R2 = L9
R3 = L10
L7 = Idris(2)
R0 = L7
R1 = L2
L7 = Idris(3)
R0 = L7
R1 = L5
L7 = Idris(3)
Idris = Array(1,L6,L7)
Case 0
Idris = Array(0)
End Select

' Prelude.Monad.Prelude.List instance of Prelude.Monad.Monad, method >>=
Case 46
Idris46:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = 0
L5 = 0
L6 = 0
L7 = Array(65695)
L8 = Array(65697)
L9 = Array(0)
L8 = Array(0,L8,L9)
R0 = L4
R1 = L5
R2 = L6
R3 = L7
R4 = L8
R5 = L3
L4 = Idris(17)
R0 = L4
R1 = L2
GoTo Idris3

' Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=
Case 132
Idris132:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(123)
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
GoTo Idris119
Case 1
Idris = Array(1)
End Select

' Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method compare
Case 118
Idris118:
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

' with block in Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <
Case 133
Idris133:
L0 = R0
L1 = R1
L2 = R2
Select Case L0(0)
Case 0
Idris = Array(1)
Case Else
Idris = Array(0)
End Select

' Prelude.List.List instance of Prelude.Functor.Functor
Case 107
Idris107:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = 0
L5 = 0
R0 = L4
R1 = L5
R2 = L2
R3 = L3
GoTo Idris121

' Prelude.Classes.Int instance of Prelude.Classes.Ord
Case 123
Idris123:
L0 = Array(65686)
L1 = Array(65688)
Idris = Array(0,L0,L1)

' case block in Void
Case 134
Idris134:
Idris = 0

' case block in io_bind
Case 135
Idris135:
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

' <<Void eliminator>>
Case 136
Idris136:
Idris = 0

End Select
End Function

' Main Entry Point
Public Sub Main
    Call Idris_InitRuntime
    Call Idris(122)
End Sub
