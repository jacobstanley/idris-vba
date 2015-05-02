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

' Prelude.List.++
Case 0
Idris0:
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
L5 = Idris(0)
Idris = Array(1,L3,L5)
Case 0
Idris = L2
End Select

' Prelude.Basics..
Case 1
Idris1:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
R0 = L4
R1 = L5
L6 = Idris(2)
R0 = L3
R1 = L6
GoTo Idris2

' Prelude.Classes.<
Case 3
Idris3:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
Idris = L3
End Select

' Prelude.Algebra.<+>
Case 4
Idris4:
L0 = R0
L1 = R1
Idris = L1

' Prelude.Monad.>>=
Case 5
Idris5:
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
L6 = Idris(2)
R0 = L6
R1 = L2
GoTo Idris2
End Select

' @@constructor of Prelude.Algebra.Monoid#Semigroup a
Case 6
Idris6:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
Idris = L2
End Select

' @@constructor of Prelude.Applicative.Alternative#Applicative f
Case 7
Idris7:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
Idris = L2
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
L3 = Idris(10)
Idris = L3

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
GoTo Idris10
Case 1
R0 = L2
GoTo Idris10
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
GoTo Idris2

' VBA.clearCells
Case 13
Idris13:
L0 = R0
Cells.DeleteIdris = 0

' Prelude.Classes.compare
Case 14
Idris14:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
Idris = L2
End Select

' Prelude.Foldable.concatMap
Case 15
Idris15:
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
L6 = Idris(16)
L7 = 0
L8 = 0
L9 = 0
Select Case L4(0)
Case 0
L10 = L4(1)
L11 = L4(2)
L10 = L10
End Select
L7 = Array(65645,L7,L8,L9,L10,L5)
R0 = L6
R1 = L7
L6 = Idris(2)
Select Case L4(0)
Case 0
L7 = L4(1)
L8 = L4(2)
L7 = L8
End Select
R0 = L6
R1 = L7
GoTo Idris2

' Prelude.Applicative.empty
Case 17
Idris17:
L0 = R0
L1 = R1
L2 = R2
Select Case L2(0)
Case 0
L3 = L2(1)
L4 = L2(2)
R0 = L4
R1 = L1
GoTo Idris2
End Select

' Prelude.Foldable.foldr
Case 16
Idris16:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
R0 = L3
R1 = L1
L4 = Idris(2)
R0 = L4
R1 = L2
GoTo Idris2

' Prelude.List.foldrImpl
Case 18
Idris18:
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
L13 = Idris(2)
L10 = Array(65645,L10,L11,L12,L4,L13)
R0 = L8
R1 = L9
R2 = L2
R3 = L3
R4 = L10
R5 = L7
GoTo Idris18
Case 0
R0 = L4
R1 = L3
GoTo Idris2
End Select

' Prelude.Applicative.guard
Case 19
Idris19:
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
GoTo Idris2
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
L3 = Idris(20)
L4 = Array(0)
R0 = L3
R1 = L4
GoTo Idris2
End Select

' Prelude.Basics.id
Case 21
Idris21:
L0 = R0
L1 = R1
Idris = L1

' Prelude.Classes.intToBool
Case 22
Idris22:
L0 = R0
Select Case L0
Case CLng(&H0&)
Idris = Array(0)
Case Else
Idris = Array(1)
End Select

' io_bind
Case 23
Idris23:
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
L6 = Idris(24)
R0 = L3
R1 = L5
L7 = Idris(2)
R0 = L6
R1 = L7
GoTo Idris2

' io_return
Case 25
Idris25:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
Idris = L2

' Main.main
Case 26
Idris26:
L0 = 0
L1 = 0
L2 = 0
L3 = 0
L4 = "Clearing cells..." & ChrW(10)
R0 = L3
R1 = L4
L3 = Idris(27)
L4 = Array(65626)
Idris = Array(65663,L0,L1,L2,L3,L4)

' Util.mapM_
Case 28
Idris28:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
Select Case L4(0)
Case 1
L5 = L4(1)
L6 = L4(2)
L7 = 0
L8 = 0
L9 = 0
R0 = L7
R1 = L8
R2 = L9
R3 = L2
L7 = Idris(5)
R0 = L3
R1 = L5
L8 = Idris(2)
R0 = L7
R1 = L8
L7 = Idris(2)
L8 = Array(65660,L2,L3,L6)
R0 = L7
R1 = L8
GoTo Idris2
Case 0
L5 = 0
L6 = 0
Select Case L2(0)
Case 0
L7 = L2(1)
L8 = L2(2)
L7 = L7
End Select
R0 = L5
R1 = L6
R2 = L7
L5 = Idris(20)
L6 = Array(0)
R0 = L5
R1 = L6
GoTo Idris2
End Select

' mkForeignPrim
Case 29
Idris29:
Idris = 0

' Prelude.Algebra.neutral
Case 30
Idris30:
L0 = R0
L1 = R1
Select Case L1(0)
Case 0
L2 = L1(1)
L3 = L1(2)
Idris = L3
End Select

' prim__addInt
Case 31
Idris31:
L0 = R0
L1 = R1
Idris = (L0 + L1)

' prim__eqInt
Case 32
Idris32:
L0 = R0
L1 = R1
Idris = (L0 = L1)

' prim__mulInt
Case 33
Idris33:
L0 = R0
L1 = R1
Idris = (L0 * L1)

' prim__sextInt_BigInt
Case 34
Idris34:
L0 = R0
Idris = CLng(L0)

' prim__sltInt
Case 35
Idris35:
L0 = R0
L1 = R1
Idris = (L0 < L1)

' prim__subInt
Case 36
Idris36:
L0 = R0
L1 = R1
Idris = (L0 - L1)

' prim__toStrInt
Case 37
Idris37:
L0 = R0
Idris = CStr(L0)

' prim__writeString
Case 38
Idris38:
L0 = R0
L1 = R1
Idris = Idris_WriteStr(L1)

' prim_io_bind
Case 39
Idris39:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
R0 = L3
R1 = L2
GoTo Idris2

' Prelude.Applicative.pure
Case 20
Idris20:
L0 = R0
L1 = R1
L2 = R2
R0 = L2
R1 = L1
GoTo Idris2

' VBA.putCell
Case 40
Idris40:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
Cells(L0,L1)=L2
Idris = 0

' Main.putResult
Case 41
Idris41:
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
L10 = Array(65662,L1,L10,L11)
L11 = Array(65637,L1,L5,L6)
Idris = Array(65663,L7,L8,L9,L10,L11)
End Select
End Select
End Select

' Prelude.putStr
Case 27
Idris27:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
L5 = Array(65658,L1)
L6 = Array(65659)
Idris = Array(65663,L2,L3,L4,L5,L6)

' Main.pythag
Case 42
Idris42:
L0 = R0
L1 = 0
L2 = 0
L3 = CLng(&H1&)
R0 = L3
R1 = L0
L3 = Idris(43)
L4 = Array(65644)
R0 = L1
R1 = L2
R2 = L3
R3 = L4
GoTo Idris44

' run__IO
Case 45
Idris45:
L0 = R0
L1 = R1
L2 = 0
R0 = L1
R1 = L2
GoTo Idris2

' unsafePerformPrimIO
Case 46
Idris46:
Idris = 0

' world
Case 47
Idris47:
L0 = R0
Idris = L0

' Main.zipIx
Case 48
Idris48:
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
L6 = Idris(48)
Idris = Array(1,L5,L6)
Case 0
Idris = Array(0)
End Select

' Prelude.Bool.||
Case 49
Idris49:
L0 = R0
L1 = R1
Select Case L0(0)
Case 0
R0 = L1
GoTo Idris10
Case 1
Idris = Array(1)
End Select

' {APPLY0}
Case 2
Idris2:
L0 = R0
L1 = R1
Select Case L0(0)
Case 65622
R0 = L1
GoTo Idris41
Case 65623
R0 = L1
GoTo Idris50
Case 65624
R0 = L1
GoTo Idris51
Case 65625
R0 = L1
GoTo Idris52
Case 65626
R0 = L1
GoTo Idris53
Case 65627
R0 = L1
GoTo Idris54
Case 65628
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris55
Case 65629
R0 = L1
GoTo Idris56
Case 65630
R0 = L1
GoTo Idris57
Case 65631
R0 = L1
GoTo Idris58
Case 65632
R0 = L1
GoTo Idris59
Case 65633
R0 = L1
GoTo Idris60
Case 65634
R0 = L1
GoTo Idris61
Case 65635
R0 = L1
GoTo Idris62
Case 65636
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris63
Case 65637
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris64
Case 65638
R0 = L1
GoTo Idris65
Case 65639
R0 = L1
GoTo Idris66
Case 65640
R0 = L1
GoTo Idris67
Case 65641
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris68
Case 65642
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris69
Case 65643
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris70
Case 65644
R0 = L1
GoTo Idris71
Case 65645
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
GoTo Idris1
Case 65646
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris21
Case 65647
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris72
Case 65648
R0 = L1
GoTo Idris73
Case 65649
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris74
Case 65650
R0 = L1
GoTo Idris75
Case 65651
L2 = L0(1)
L3 = L0(2)
R0 = L2
R1 = L3
R2 = L1
GoTo Idris76
Case 65652
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris77
Case 65653
R0 = L1
GoTo Idris78
Case 65654
R0 = L1
GoTo Idris79
Case 65655
R0 = L1
GoTo Idris80
Case 65656
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris81
Case 65657
R0 = L1
GoTo Idris82
Case 65658
L2 = L0(1)
R0 = L2
R1 = L1
GoTo Idris83
Case 65659
R0 = L1
GoTo Idris84
Case 65660
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris85
Case 65661
R0 = L1
GoTo Idris13
Case 65662
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris40
Case 65663
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
GoTo Idris23
Case 65664
L2 = L0(1)
L3 = L0(2)
L4 = L0(3)
R0 = L2
R1 = L3
R2 = L4
R3 = L1
GoTo Idris25
Case 65665
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
GoTo Idris86
Case Else
Idris = 0
End Select

' {EVAL0}
Case 10
Idris10:
L0 = R0
Select Case L0
Case Else
Idris = L0
End Select

' Prelude.Classes.{Int instance of Prelude.Classes.Ord0}
Case 72
Idris72:
L0 = R0
L1 = R1
R0 = L0
R1 = L1
GoTo Idris87

' Prelude.Classes.{Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=0}
Case 88
Idris88:
L0 = R0
L1 = R1
L2 = (L0 = L1)
Select Case L2
Case CLng(&H0&)
Idris = Array(0)
Case Else
Idris = Array(1)
End Select

' Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=0}
Case 76
Idris76:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
L5 = Array(65646,L5)
R0 = L3
R1 = L4
R2 = L0
R3 = L1
R4 = L5
R5 = L2
GoTo Idris18

' {io_bind0}
Case 89
Idris89:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
L6 = R6
R0 = L4
R1 = L6
GoTo Idris2

' Main.{main0}
Case 50
Idris50:
L0 = R0
L1 = 0
L2 = 0
Idris = Array(65664,L1,L2,L0)

' Util.{mapM_0}
Case 85
Idris85:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = 0
L5 = 0
R0 = L4
R1 = L5
R2 = L0
R3 = L1
R4 = L2
GoTo Idris28

' Main.{putResult0}
Case 63
Idris63:
L0 = R0
L1 = R1
L2 = R2
L3 = CLng(&H3&)
L4 = CStr(L1)
Idris = Array(65662,L0,L3,L4)

' Prelude.{putStr0}
Case 83
Idris83:
L0 = R0
L1 = R1
Idris = Idris_WriteStr(L0)

' Main.{pythag0}
Case 65
Idris65:
L0 = R0
L1 = Array(0)
Idris = Array(1,L0,L1)

' {runMain0}
Case 90
Idris90:
L0 = Idris(26)
L1 = 0
R0 = L0
R1 = L1
L0 = Idris(2)
R0 = L0
GoTo Idris10

' Prelude.Classes.{Int instance of Prelude.Classes.Ord1}
Case 73
Idris73:
L0 = R0
Idris = Array(65647,L0)

' Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=1}
Case 77
Idris77:
L0 = R0
L1 = R1
Idris = Array(65651,L0,L1)

' {io_bind1}
Case 86
Idris86:
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
L7 = Idris(89)
R0 = L7
R1 = L5
GoTo Idris2

' Main.{main1}
Case 54
Idris54:
L0 = R0
Idris = Array(65623)

' Main.{putResult1}
Case 64
Idris64:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = 0
L5 = 0
L6 = 0
L7 = CLng(&H2&)
L8 = CStr(L1)
L7 = Array(65662,L0,L7,L8)
L8 = Array(65636,L0,L2)
Idris = Array(65663,L4,L5,L6,L7,L8)

' Prelude.{putStr1}
Case 84
Idris84:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(0)
Idris = Array(65664,L1,L2,L3)

' Main.{pythag1}
Case 66
Idris66:
L0 = R0
Idris = Array(65638)

' Prelude.Classes.{Int instance of Prelude.Classes.Ord2}
Case 74
Idris74:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(91)
R0 = L2
R1 = L3
L2 = Idris(14)
R0 = L2
R1 = L0
L2 = Idris(2)
R0 = L2
R1 = L1
L2 = Idris(2)
Select Case L2(0)
Case 0
Idris = Array(1)
Case Else
Idris = Array(0)
End Select

' Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=2}
Case 78
Idris78:
L0 = R0
Idris = Array(65652,L0)

' {io_bind2}
Case 24
Idris24:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
L5 = R5
Idris = Array(65665,L0,L1,L2,L3,L4,L5)

' Main.{main2}
Case 55
Idris55:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = 0
Idris = Array(65663,L2,L3,L4,L0,L1)

' Main.{pythag2}
Case 67
Idris67:
L0 = R0
Idris = Array(0)

' Prelude.Classes.{Int instance of Prelude.Classes.Ord3}
Case 75
Idris75:
L0 = R0
Idris = Array(65649,L0)

' Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=3}
Case 79
Idris79:
L0 = R0
Idris = Array(65653)

' Main.{main3}
Case 56
Idris56:
L0 = R0
Idris = Array(65628,L0)

' Main.{pythag3}
Case 68
Idris68:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = Array(0,L1,L2)
L4 = Array(0,L0,L4)
L5 = Array(0)
Idris = Array(1,L4,L5)

' Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=4}
Case 80
Idris80:
L0 = R0
Idris = Array(65654)

' Main.{main4}
Case 57
Idris57:
L0 = R0
Idris = Array(65629)

' Main.{pythag4}
Case 69
Idris69:
L0 = R0
L1 = R1
L2 = R2
L3 = 0
L4 = 0
L5 = 0
L6 = Array(65639)
L7 = Array(65640)
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
L5 = Idris(19)
L6 = Array(65641,L2,L0,L1)
R0 = L3
R1 = L4
R2 = L5
R3 = L6
GoTo Idris44

' Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=5}
Case 81
Idris81:
L0 = R0
L1 = R1
L2 = 0
R0 = L2
R1 = L0
R2 = L1
GoTo Idris0

' Main.{main5}
Case 58
Idris58:
L0 = R0
Idris = Array(65630)

' Main.{pythag5}
Case 70
Idris70:
L0 = R0
L1 = R1
L2 = 0
L3 = 0
L4 = CLng(&H1&)
R0 = L4
R1 = L1
L4 = Idris(43)
L5 = Array(65642,L1,L0)
R0 = L2
R1 = L3
R2 = L4
R3 = L5
GoTo Idris44

' Prelude.Monad.{Prelude.List instance of Prelude.Monad.Monad, method >>=6}
Case 82
Idris82:
L0 = R0
Idris = Array(65656,L0)

' Main.{main6}
Case 59
Idris59:
L0 = R0
L1 = 0
L2 = 0
L3 = Array(65627)
L4 = Array(65631)
L3 = Array(0,L3,L4)
L4 = Array(65622)
L5 = 0
L6 = CLng(&H2&)
L7 = CLng(&H32&)
R0 = L7
L7 = Idris(42)
R0 = L5
R1 = L6
R2 = L7
L5 = Idris(48)
R0 = L1
R1 = L2
R2 = L3
R3 = L4
R4 = L5
GoTo Idris28

' Main.{pythag6}
Case 71
Idris71:
L0 = R0
L1 = 0
L2 = 0
L3 = CLng(&H1&)
R0 = L3
R1 = L0
L3 = Idris(43)
L4 = Array(65643,L0)
R0 = L1
R1 = L2
R2 = L3
R3 = L4
GoTo Idris44

' Main.{main7}
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
L4 = Idris(27)
L5 = Array(65632)
Idris = Array(65663,L1,L2,L3,L4,L5)

' Main.{main8}
Case 61
Idris61:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = CLng(&H1&)
L5 = CLng(&H3&)
L6 = "Z"
L4 = Array(65662,L4,L5,L6)
L5 = Array(65633)
Idris = Array(65663,L1,L2,L3,L4,L5)

' Main.{main9}
Case 62
Idris62:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = CLng(&H1&)
L5 = CLng(&H2&)
L6 = "Y"
L4 = Array(65662,L4,L5,L6)
L5 = Array(65634)
Idris = Array(65663,L1,L2,L3,L4,L5)

' Main.{main10}
Case 51
Idris51:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = CLng(&H1&)
L5 = CLng(&H1&)
L6 = "X"
L4 = Array(65662,L4,L5,L6)
L5 = Array(65635)
Idris = Array(65663,L1,L2,L3,L4,L5)

' Main.{main11}
Case 52
Idris52:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = 0
L5 = "Writing headers..." & ChrW(10)
R0 = L4
R1 = L5
L4 = Idris(27)
L5 = Array(65624)
Idris = Array(65663,L1,L2,L3,L4,L5)

' Main.{main12}
Case 53
Idris53:
L0 = R0
L1 = 0
L2 = 0
L3 = 0
L4 = Array(65661)
L5 = Array(65625)
Idris = Array(65663,L1,L2,L3,L4,L5)

' Decidable.Equality.Decidable.Equality.Char instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 92
Idris92:
Idris = 0

' Decidable.Equality.Decidable.Equality.Int instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 93
Idris93:
Idris = 0

' Decidable.Equality.Decidable.Equality.Integer instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 94
Idris94:
Idris = 0

' Decidable.Equality.Decidable.Equality.String instance of Decidable.Equality.DecEq, method decEq, primitiveNotEq
Case 95
Idris95:
Idris = 0

' Prelude.Prelude.Int instance of Prelude.Enum, method enumFromTo, go
Case 96
Idris96:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = R4
Select Case L3
Case CLng(&H0&)
Idris = Array(1,L4,L2)
Case Else
L5 = CLng(&H1&)
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
GoTo Idris96
End Select

' Prelude.Prelude.Int instance of Prelude.Enum, method enumFromTo
Case 43
Idris43:
L0 = R0
L1 = R1
R0 = L0
R1 = L1
L2 = Idris(97)
Select Case L2(0)
Case 0
Idris = Array(0)
Case 1
L3 = 0
L4 = 0
L5 = Array(0)
L6 = (L1 - L0)
L6 = CLng(L6)
R0 = L3
R1 = L4
R2 = L5
R3 = L6
R4 = L1
GoTo Idris96
End Select

' Prelude.Monad.Prelude.List instance of Prelude.Monad.Monad, method >>=
Case 44
Idris44:
L0 = R0
L1 = R1
L2 = R2
L3 = R3
L4 = 0
L5 = 0
L6 = 0
L7 = Array(65655)
L8 = Array(65657)
L9 = Array(0)
L8 = Array(0,L8,L9)
R0 = L4
R1 = L5
R2 = L6
R3 = L7
R4 = L8
R5 = L3
L4 = Idris(15)
R0 = L4
R1 = L2
GoTo Idris2

' Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method <=
Case 97
Idris97:
L0 = R0
L1 = R1
L2 = 0
L3 = Idris(91)
R0 = L2
R1 = L3
L2 = Idris(3)
R0 = L2
R1 = L0
L2 = Idris(2)
R0 = L2
R1 = L1
L2 = Idris(2)
Select Case L2(0)
Case 0
R0 = L0
R1 = L1
GoTo Idris88
Case 1
Idris = Array(1)
End Select

' Prelude.Classes.Prelude.Classes.Int instance of Prelude.Classes.Ord, method compare
Case 87
Idris87:
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
Case 98
Idris98:
L0 = R0
L1 = R1
L2 = R2
Select Case L0(0)
Case 0
Idris = Array(1)
Case Else
Idris = Array(0)
End Select

' Prelude.Classes.Int instance of Prelude.Classes.Ord
Case 91
Idris91:
L0 = Array(65648)
L1 = Array(65650)
Idris = Array(0,L0,L1)

' case block in Void
Case 99
Idris99:
Idris = 0

' case block in io_bind
Case 100
Idris100:
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
GoTo Idris2

' <<Void eliminator>>
Case 101
Idris101:
Idris = 0

End Select
End Function

' Main Entry Point
Public Sub Main
    Idris_MakeOnBits8
    Idris_MakeOnBits16
    Idris_MakeOnBits32
    Call Idris(90)
End Sub
