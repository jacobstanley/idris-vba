' Foreign Functions
Private Declare Function FFI68 Lib "libc.dylib" Alias "htons" (ByVal arg1 As Integer) As Integer
Private Declare Function FFI67 Lib "libc.dylib" Alias "htonl" (ByVal arg1 As Long) As Long

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

' Bit patterns for shift left operations
Private Idris_OnBits8(0 To 7) As Byte
Private Idris_OnBits16(0 To 15) As Integer
Private Idris_OnBits32(0 To 31) As Long

' Runtime Functions
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
    Idris_OnBits32(j) = v + &H80000000
End Sub

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

Private Function Idris_LShr8(ByVal value As Byte, ByVal shift As Integer) As Integer
    Dim hi As Long
    If (value And &H80) Then hi = &H40
    Idris_LShr8 = (value And &H7E) \ (2 ^ shift)
    Idris_LShr8 = (Idris_LShr8 Or (hi \ (2 ^ (shift - 1))))
End Function

Private Function Idris_LShr16(ByVal value As Integer, ByVal shift As Integer) As Integer
    Dim hi As Long
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

Private Function Idris_UTrunc16(ByVal value As Long) As Integer
    Dim masked As Long
    masked = value And &HFFFF&
    If masked < &H8000& Then
        Idris_UTrunc16 = CInt(masked)
    Else
        Idris_UTrunc16 = CInt(masked - &H10000)
    End If
End Function

Function Idris_UGt16(ByVal x As Integer, ByVal y As Integer) As Boolean
    Idris_UGt16 = (x > y) Xor (x < 0) Xor (y < 0)
End Function

Function Idris_UGe16(ByVal x As Integer, ByVal y As Integer) As Boolean
    Idris_UGe16 = (x >= y) Xor (x < 0) Xor (y < 0)
End Function

Function Idris_ULt16(ByVal x As Integer, ByVal y As Integer) As Boolean
    Idris_ULt16 = (x < y) Xor (x < 0) Xor (y < 0)
End Function

Function Idris_ULe16(ByVal x As Integer, ByVal y As Integer) As Boolean
    Idris_ULe16 = (x <= y) Xor (x < 0) Xor (y < 0)
End Function

Function Idris_UGt32(ByVal x As Long, ByVal y As Long) As Boolean
    Idris_UGt32 = (x > y) Xor (x < 0) Xor (y < 0)
End Function

Function Idris_UGe32(ByVal x As Long, ByVal y As Long) As Boolean
    Idris_UGe32 = (x >= y) Xor (x < 0) Xor (y < 0)
End Function

Function Idris_ULt32(ByVal x As Long, ByVal y As Long) As Boolean
    Idris_ULt32 = (x < y) Xor (x < 0) Xor (y < 0)
End Function

Function Idris_ULe32(ByVal x As Long, ByVal y As Long) As Boolean
    Idris_ULe32 = (x <= y) Xor (x < 0) Xor (y < 0)
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

Private Function Idris_Append(xs, ys) As String
    Idris_Append = CStr(xs) + CStr(ys)
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

' Prelude.Classes.>
Case 5
Idris5:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
L4 = L1(3)
Idris = L4
End Select

' Force
Case 6
Idris6:
L0 = R0
L1 = R1
L2 = R2
R0 = L2
L3 = Idris(1)
Idris = L3

' Prelude.Bits.b8ToString
Case 7
Idris7:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(65642)
L4 = ""
L5 = 0
L5 = Array(65626,L5)
R0 = L0
L6 = Idris(8)
L7 = 0
L8 = CByte(&Hf)
L8 = (L0 And L8)
R0 = L7
R1 = L8
L7 = Idris(9)
L8 = Array(0)
L7 = Array(1,L7,L8)
L6 = Array(1,L6,L7)
R0 = L1
R1 = L2
R2 = L3
R3 = L4
R4 = L5
R5 = L6
GoTo Idris10

' Prelude.Bool.boolElim
Case 11
Idris11:
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
Case 12
Idris12:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
R0 = L2
R1 = L3
GoTo Idris3

' Prelude.Classes.compare
Case 13
Idris13:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
L4 = L1(3)
Idris = L2
End Select

' Prelude.List.foldrImpl
Case 10
Idris10:
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
L10 = Array(65625,L10,L11,L12,L4,L13)
R0 = L8
R1 = L9
R2 = L2
R3 = L3
R4 = L10
R5 = L7
GoTo Idris10
Case 0
R0 = L4
R1 = L3
GoTo Idris3
End Select

' VBA.htonl
Case 14
Idris14:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(65635,L0)
R0 = L1
R1 = L2
R2 = L3
GoTo Idris15

' VBA.htons
Case 16
Idris16:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(65636,L0)
R0 = L1
R1 = L2
R2 = L3
GoTo Idris15

' Prelude.Basics.id
Case 17
Idris17:
L0 = R0
L1 = R1
Idris = L1

' Prelude.Classes.intToBool
Case 18
Idris18:
L0 = R0
Select Case L0
Case CLng(&H0&)
Idris = Array(0)
Case Else
Idris = Array(1)
End Select

' io_bind
Case 19
Idris19:
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
L6 = Idris(20)
R0 = L3
R1 = L5
L7 = Idris(3)
R0 = L6
R1 = L7
GoTo Idris3

' io_return
Case 21
Idris21:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
Idris = L2

' Main.main
Case 22
Idris22:
L0 = CInt(&H1234)
L1 = CLng(&H1234abcd)
L2 = 0
L3 = 0
L4 = 0
L5 = 0
L6 = "h16 = "
L7 = CInt(&H8)
L7 = Idris_LShr16(L0, L7)
L7 = CByte(L7 And &HFF)
R0 = L7
L7 = Idris(7)
L8 = CByte(L0 And &HFF)
R0 = L8
L8 = Idris(7)
L7 = Idris_Append(L7, L8)
L6 = Idris_Append(L6, L7)
L7 = ChrW(10)
L6 = Idris_Append(L6, L7)
R0 = L5
R1 = L6
L5 = Idris(23)
L6 = Array(65624,L1,L0)
Idris = Array(65637,L2,L3,L4,L5,L6)

' mkForeignPrim
Case 24
Idris24:
Idris = 0

' prim__addChar
Case 25
Idris25:
L0 = R0
L1 = R1
Idris = (L0 + L1)

' prim__andB32
Case 26
Idris26:
L0 = R0
L1 = R1
Idris = (L0 And L1)

' prim__andB8
Case 27
Idris27:
L0 = R0
L1 = R1
Idris = (L0 And L1)

' prim__concat
Case 28
Idris28:
L0 = R0
L1 = R1
Idris = Idris_Append(L0, L1)

' prim__eqInt
Case 29
Idris29:
L0 = R0
L1 = R1
Idris = (L0 = L1)

' prim__intToChar
Case 30
Idris30:
L0 = R0
Idris = L0

' prim__lshrB16
Case 31
Idris31:
L0 = R0
L1 = R1
Idris = Idris_LShr16(L0, L1)

' prim__lshrB32
Case 32
Idris32:
L0 = R0
L1 = R1
Idris = Idris_LShr32(L0, L1)

' prim__lshrB8
Case 33
Idris33:
L0 = R0
L1 = R1
Idris = Idris_LShr8(L0, L1)

' prim__sltInt
Case 34
Idris34:
L0 = R0
L1 = R1
Idris = (L0 < L1)

' prim__strCons
Case 35
Idris35:
L0 = R0
L1 = R1
Idris = Idris_Append(Chr(L0), L1)

' prim__subInt
Case 36
Idris36:
L0 = R0
L1 = R1
Idris = (L0 - L1)

' prim__truncB16_B8
Case 37
Idris37:
L0 = R0
Idris = CByte(L0 And &HFF)

' prim__truncB32_B16
Case 38
Idris38:
L0 = R0
Idris = Idris_UTrunc16(L0)

' prim__writeString
Case 39
Idris39:
L0 = R0
L1 = R1
Idris = Idris_WriteStr(L1)

' prim__zextB8_Int
Case 40
Idris40:
L0 = R0
Idris = CLng(L0 And &HFF&)

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

' Prelude.putStr
Case 23
Idris23:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = Array(65633,L1)
L6 = Array(65634)
Idris = Array(65637,L2,L3,L4,L5,L6)

' run__IO
Case 42
Idris42:
L0 = R0
L1 = R1
L2 = 0
R0 = L1
R1 = L2
GoTo Idris3

' unsafePerformIO
Case 15
Idris15:
L0 = R0
L1 = R1
L2 = R2
R0 = L0
R1 = L1
R2 = L2
L3 = Idris(43)
L4 = 0
R0 = L2
R1 = L4
L4 = Idris(3)
R0 = L3
R1 = L4
GoTo Idris3

' unsafePerformPrimIO
Case 44
Idris44:
Idris = 0

' world
Case 45
Idris45:
L0 = R0
Idris = L0

' Prelude.Bool.||
Case 46
Idris46:
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
Case 65622
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris47
Case 65623
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris48
Case 65624
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris49
Case 65625
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
Case 65626
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris17
Case 65627
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris50
Case 65628
R0 = L1
GoTo Idris51
Case 65629
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris52
Case 65630
R0 = L1
GoTo Idris53
Case 65631
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris54
Case 65632
R0 = L1
GoTo Idris55
Case 65633
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris56
Case 65634
R0 = L1
GoTo Idris57
Case 65635
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris58
Case 65636
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris59
Case 65637
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
GoTo Idris19
Case 65638
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris21
Case 65639
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris35
Case 65640
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
GoTo Idris60
Case 65641
R0 = L1
GoTo Idris61
Case 65642
Idris = Array(65639,L1)
Case Else
Idris = 0
End Select

' {EVAL0}
Case 1
Idris1:
L0 = R0
Select Case L0
Case Else
Idris = L0
End Select

' Prelude.Classes.{Int instance of Prelude.Classes.Ord0}
Case 50
Idris50:
L0 = R0
L1 = R1
R0 = L0
R1 = L1
GoTo Idris62

' {Prelude.Bits.b8ToString, getDigit0}
Case 63
Idris63:
L0 = R0
L1 = CLng(&Hf&)
R0 = L0
R1 = L1
GoTo Idris64

' Prelude.Classes.{Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=0}
Case 65
Idris65:
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
Case 66
Idris66:
L0 = R0
L1 = R1
L2 = (L0 = L1)
Select Case L2
Case CLng(&H0&)
Idris = Array(0)
Case Else
Idris = Array(1)
End Select

' VBA.{htonl0}
Case 58
Idris58:
L0 = R0
L1 = R1
Idris = FFI67(L0)

' VBA.{htons0}
Case 59
Idris59:
L0 = R0
L1 = R1
Idris = FFI68(L0)

' {io_bind0}
Case 69
Idris69:
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
Case 47
Idris47:
L0 = R0
L1 = R1
L2 = 0
L3 = "n32 = "
R0 = L0
L4 = Idris(14)
R0 = L4
L4 = Idris(70)
L5 = CInt(&H8)
L4 = Idris_LShr16(L4, L5)
L4 = CByte(L4 And &HFF)
R0 = L4
L4 = Idris(7)
R0 = L0
L5 = Idris(14)
R0 = L5
L5 = Idris(70)
L5 = CByte(L5 And &HFF)
R0 = L5
L5 = Idris(7)
L4 = Idris_Append(L4, L5)
R0 = L0
L5 = Idris(14)
L5 = Idris_UTrunc16(L5)
L6 = CInt(&H8)
L5 = Idris_LShr16(L5, L6)
L5 = CByte(L5 And &HFF)
R0 = L5
L5 = Idris(7)
R0 = L0
L6 = Idris(14)
L6 = Idris_UTrunc16(L6)
L6 = CByte(L6 And &HFF)
R0 = L6
L6 = Idris(7)
L5 = Idris_Append(L5, L6)
L4 = Idris_Append(L4, L5)
L3 = Idris_Append(L3, L4)
L4 = ChrW(10)
L3 = Idris_Append(L3, L4)
R0 = L2
R1 = L3
GoTo Idris23

' Prelude.{putStr0}
Case 56
Idris56:
L0 = R0
L1 = R1
Idris = Idris_WriteStr(L0)

' {runMain0}
Case 71
Idris71:
L0 = Idris(22)
L1 = 0
R0 = L0
R1 = L1
L0 = Idris(3)
R0 = L0
GoTo Idris1

' {unsafePerformIO0}
Case 61
Idris61:
L0 = R0
Idris = L0

' Prelude.Classes.{Int instance of Prelude.Classes.Ord1}
Case 51
Idris51:
L0 = R0
Idris = Array(65627,L0)

' {Prelude.Bits.b8ToString, getDigit1}
Case 72
Idris72:
L0 = R0
L1 = CLng(&H9&)
R0 = L0
R1 = L1
GoTo Idris64

' {io_bind1}
Case 60
Idris60:
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
L7 = Idris(69)
R0 = L7
R1 = L5
GoTo Idris3

' Main.{main1}
Case 48
Idris48:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
L6 = 0
L7 = "n16 = "
R0 = L0
L8 = Idris(16)
L9 = CInt(&H8)
L8 = Idris_LShr16(L8, L9)
L8 = CByte(L8 And &HFF)
R0 = L8
L8 = Idris(7)
R0 = L0
L9 = Idris(16)
L9 = CByte(L9 And &HFF)
R0 = L9
L9 = Idris(7)
L8 = Idris_Append(L8, L9)
L7 = Idris_Append(L7, L8)
L8 = ChrW(10)
L7 = Idris_Append(L7, L8)
R0 = L6
R1 = L7
L6 = Idris(23)
L7 = Array(65622,L1)
Idris = Array(65637,L3,L4,L5,L6,L7)

' Prelude.{putStr1}
Case 57
Idris57:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(0)
Idris = Array(65638,L1,L2,L3)

' {unsafePerformIO1}
Case 43
Idris43:
L0 = R0
L1 = R1
L2 = R2
Idris = Array(65641)

' Prelude.Classes.{Int instance of Prelude.Classes.Ord2}
Case 52
Idris52:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(73)
R0 = L2
R1 = L3
L2 = Idris(13)
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

' {io_bind2}
Case 20
Idris20:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
Idris = Array(65640,L0,L1,L2,L3,L4,L5)

' Main.{main2}
Case 49
Idris49:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
L6 = 0
L7 = "h32 = "
R0 = L0
L8 = Idris(70)
L9 = CInt(&H8)
L8 = Idris_LShr16(L8, L9)
L8 = CByte(L8 And &HFF)
R0 = L8
L8 = Idris(7)
R0 = L0
L9 = Idris(70)
L9 = CByte(L9 And &HFF)
R0 = L9
L9 = Idris(7)
L8 = Idris_Append(L8, L9)
L9 = Idris_UTrunc16(L0)
L10 = CInt(&H8)
L9 = Idris_LShr16(L9, L10)
L9 = CByte(L9 And &HFF)
R0 = L9
L9 = Idris(7)
L10 = Idris_UTrunc16(L0)
L10 = CByte(L10 And &HFF)
R0 = L10
L10 = Idris(7)
L9 = Idris_Append(L9, L10)
L8 = Idris_Append(L8, L9)
L7 = Idris_Append(L7, L8)
L8 = ChrW(10)
L7 = Idris_Append(L7, L8)
R0 = L6
R1 = L7
L6 = Idris(23)
L7 = Array(65623,L1,L0)
Idris = Array(65637,L3,L4,L5,L6,L7)

' Prelude.Classes.{Int instance of Prelude.Classes.Ord3}
Case 53
Idris53:
L0 = R0
Idris = Array(65629,L0)

' Prelude.Classes.{Int instance of Prelude.Classes.Ord4}
Case 54
Idris54:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(73)
R0 = L2
R1 = L3
L2 = Idris(13)
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

' Prelude.Classes.{Int instance of Prelude.Classes.Ord5}
Case 55
Idris55:
L0 = R0
Idris = Array(65631,L0)

' Prelude.Bits.b8ToString, c1
Case 8
Idris8:
L0 = R0
L1 = 0
L2 = CByte(&H4)
L2 = Idris_LShr8(L0, L2)
L3 = CByte(&Hf)
L2 = (L2 And L3)
R0 = L1
R1 = L2
GoTo Idris9

' Prelude.Bits.b8ToString, getDigit
Case 9
Idris9:
L0 = R0
L1 = R1
L2 = CLng(L1 And &HFF&)
L3 = CLng(&H0&)
R0 = L2
R1 = L3
L3 = Idris(74)
Select Case L3(0)
Case 0
L3 = Array(0)
Case 1
R0 = L2
L3 = Idris(72)
End Select
Select Case L3(0)
Case 0
L4 = CLng(&Ha&)
R0 = L2
R1 = L4
L4 = Idris(74)
Select Case L4(0)
Case 0
L4 = Array(0)
Case 1
R0 = L2
L4 = Idris(63)
End Select
Select Case L4(0)
Case 0
Idris = CInt(&H3f)
Case 1
L5 = CInt(&H41)
L6 = CLng(&Ha&)
L6 = (L2 - L6)
L6 = L6
Idris = (L5 + L6)
End Select
Case 1
L4 = CInt(&H30)
L5 = L2
Idris = (L4 + L5)
End Select

' Decidable.Equality.Decidable.Equality.Char instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 75
Idris75:
Idris = 0

' Decidable.Equality.Decidable.Equality.Int instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 76
Idris76:
Idris = 0

' Decidable.Equality.Decidable.Equality.Integer instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 77
Idris77:
Idris = 0

' Decidable.Equality.Decidable.Equality.String instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 78
Idris78:
Idris = 0

' Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=
Case 64
Idris64:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(73)
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
GoTo Idris65
Case 1
Idris = Array(1)
End Select

' Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method >=
Case 74
Idris74:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(73)
R0 = L2
R1 = L3
L2 = Idris(5)
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
GoTo Idris66
Case 1
Idris = Array(1)
End Select

' Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method compare
Case 62
Idris62:
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

' Prelude.Bits.b32ToString, c1, upper
Case 70
Idris70:
L0 = R0
L1 = CLng(&H10)
L1 = Idris_LShr32(L0, L1)
L2 = CLng(&Hffff)
L1 = (L1 And L2)
Idris = Idris_UTrunc16(L1)

' with block in Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <
Case 79
Idris79:
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
Case 80
Idris80:
L0 = R0
L1 = R1
L2 = R2
Select Case L0(0)
Case 2
Idris = Array(1)
Case Else
Idris = Array(0)
End Select

' Prelude.Classes.Int instance of Prelude.Classes.Ord
Case 73
Idris73:
L0 = Array(65628)
L1 = Array(65630)
L2 = Array(65632)
Idris = Array(0,L0,L1,L2)

' case block in Void
Case 81
Idris81:
Idris = 0

' case block in io_bind
Case 82
Idris82:
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
Case 83
Idris83:
Idris = 0

End Select
End Function

' Main Entry Point
Public Sub Main
    Idris_MakeOnBits8
    Idris_MakeOnBits16
    Idris_MakeOnBits32
    Call Idris(71)
End Sub
