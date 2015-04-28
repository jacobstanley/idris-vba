Function Idris_Error(msg)
    Call Err.Raise(10101, "Idris", msg)
End Function

Function Idris_WriteStr(str)
    Debug.Print str
End Function

Function Idris_ReadStr() As String
    Idris_ReadStr = InputBox("Input:", "Idris")
End Function

Function Idris_Append(xs, ys) As String
    Idris_Append = CStr(xs) + CStr(ys)
End Function

Function Idris_Prelude_46_List_46__43__43_(Loc0,Loc1,Loc2)
Select Case Loc1(0)
Case 1
Loc3 = Loc1(1)
Loc4 = Loc1(2)
Loc5 = 0
Loc5 = Idris_Prelude_46_List_46__43__43_(Loc5,Loc4,Loc2)
Idris_Prelude_46_List_46__43__43_ = Array(1,Loc3,Loc5)
Exit Function
Case 0

Idris_Prelude_46_List_46__43__43_ = Loc2
Exit Function
End Select

End Function

Function Idris_Prelude_46_Basics_46__46_(Loc0,Loc1,Loc2,Loc3,Loc4,Loc5)
Loc6 = Idris__123_APPLY0_125_(Loc4,Loc5)
Idris_Prelude_46_Basics_46__46_ = Idris__123_APPLY0_125_(Loc3,Loc6)
Exit Function
End Function

Function Idris_Prelude_46_Classes_46__60_(Loc0,Loc1)
Select Case Loc1(0)
Case 0
Loc2 = Loc1(1)
Loc3 = Loc1(2)
Idris_Prelude_46_Classes_46__60_ = Loc3
Exit Function
End Select

End Function

Function Idris_Prelude_46_Algebra_46__60__43__62_(Loc0,Loc1)
Idris_Prelude_46_Algebra_46__60__43__62_ = Loc1
Exit Function
End Function

Function Idris__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a(Loc0,Loc1)
Select Case Loc1(0)
Case 0
Loc2 = Loc1(1)
Loc3 = Loc1(2)
Idris__64__64_constructor_32_of_32_Prelude_46_Algebra_46_Monoid_35_Semigroup_32_a = Loc2
Exit Function
End Select

End Function

Function Idris__64__64_constructor_32_of_32_Prelude_46_Applicative_46_Alternative_35_Applicative_32_f(Loc0,Loc1)
Select Case Loc1(0)
Case 0
Loc2 = Loc1(1)
Loc3 = Loc1(2)
Idris__64__64_constructor_32_of_32_Prelude_46_Applicative_46_Alternative_35_Applicative_32_f = Loc2
Exit Function
End Select

End Function

Function Idris_Force(Loc0,Loc1,Loc2)
Loc3 = Idris__123_EVAL0_125_(Loc2)
Idris_Force = Loc3
Exit Function
End Function

Function Idris_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c(Loc0)
Loc1 = 0
Loc2 = 0
Loc3 = Array(65649)
Idris_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c = Idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0(Loc1,Loc2,Loc3,Loc0)
Exit Function
End Function

Function Idris_Prelude_46_Bool_46_boolElim(Loc0,Loc1,Loc2,Loc3)
Select Case Loc1(0)
Case 0

Idris_Prelude_46_Bool_46_boolElim = Idris__123_EVAL0_125_(Loc3)
Exit Function
Case 1

Idris_Prelude_46_Bool_46_boolElim = Idris__123_EVAL0_125_(Loc2)
Exit Function
End Select

End Function

Function Idris_call_95__95_IO(Loc0,Loc1,Loc2)
Loc3 = 0
Idris_call_95__95_IO = Idris__123_APPLY0_125_(Loc2,Loc3)
Exit Function
End Function

Function Idris_Prelude_46_Classes_46_compare(Loc0,Loc1)
Select Case Loc1(0)
Case 0
Loc2 = Loc1(1)
Loc3 = Loc1(2)
Idris_Prelude_46_Classes_46_compare = Loc2
Exit Function
End Select

End Function

Function Idris_Prelude_46_Foldable_46_concatMap(Loc0,Loc1,Loc2,Loc3,Loc4,Loc5)
Loc6 = 0
Loc7 = 0
Loc8 = 0
Loc6 = Idris_Prelude_46_Foldable_46_foldr(Loc6,Loc7,Loc8,Loc3)
Loc7 = 0
Loc8 = 0
Loc9 = 0
Select Case Loc4(0)
Case 0
Loc10 = Loc4(1)
Loc11 = Loc4(2)
Loc10 = Loc10

End Select
Loc7 = Array(65628,Loc7,Loc8,Loc9,Loc10,Loc5)
Loc6 = Idris__123_APPLY0_125_(Loc6,Loc7)
Select Case Loc4(0)
Case 0
Loc7 = Loc4(1)
Loc8 = Loc4(2)
Loc7 = Loc8

End Select
Idris_Prelude_46_Foldable_46_concatMap = Idris__123_APPLY0_125_(Loc6,Loc7)
Exit Function
End Function

Function Idris_Prelude_46_Applicative_46_empty(Loc0,Loc1,Loc2)
Select Case Loc2(0)
Case 0
Loc3 = Loc2(1)
Loc4 = Loc2(2)
Idris_Prelude_46_Applicative_46_empty = Idris__123_APPLY0_125_(Loc4,Loc1)
Exit Function
End Select

End Function

Function Idris_Prelude_46_Foldable_46_foldr(Loc0,Loc1,Loc2,Loc3)
Loc4 = Idris__123_APPLY0_125_(Loc3,Loc1)
Idris_Prelude_46_Foldable_46_foldr = Idris__123_APPLY0_125_(Loc4,Loc2)
Exit Function
End Function

Function Idris_Prelude_46_List_46_foldrImpl(Loc0,Loc1,Loc2,Loc3,Loc4,Loc5)
Select Case Loc5(0)
Case 1
Loc6 = Loc5(1)
Loc7 = Loc5(2)
Loc8 = 0
Loc9 = 0
Loc10 = 0
Loc11 = 0
Loc12 = 0
Loc13 = Idris__123_APPLY0_125_(Loc2,Loc6)
Loc10 = Array(65628,Loc10,Loc11,Loc12,Loc4,Loc13)
Idris_Prelude_46_List_46_foldrImpl = Idris_Prelude_46_List_46_foldrImpl(Loc8,Loc9,Loc2,Loc3,Loc10,Loc7)
Exit Function
Case 0

Idris_Prelude_46_List_46_foldrImpl = Idris__123_APPLY0_125_(Loc4,Loc3)
Exit Function
End Select

End Function

Function Idris_Prelude_46_Applicative_46_guard(Loc0,Loc1,Loc2)
Select Case Loc2(0)
Case 0

Select Case Loc1(0)
Case 0
Loc3 = Loc1(1)
Loc4 = Loc1(2)
Loc5 = 0
Idris_Prelude_46_Applicative_46_guard = Idris__123_APPLY0_125_(Loc4,Loc5)
Exit Function
End Select

Case 1

Loc3 = 0
Loc4 = 0
Select Case Loc1(0)
Case 0
Loc5 = Loc1(1)
Loc6 = Loc1(2)
Loc5 = Loc5

End Select
Loc3 = Idris_Prelude_46_Applicative_46_pure(Loc3,Loc4,Loc5)
Loc4 = Array(0)
Idris_Prelude_46_Applicative_46_guard = Idris__123_APPLY0_125_(Loc3,Loc4)
Exit Function
End Select

End Function

Function Idris_Prelude_46_Basics_46_id(Loc0,Loc1)
Idris_Prelude_46_Basics_46_id = Loc1
Exit Function
End Function

Function Idris_Prelude_46_Classes_46_intToBool(Loc0)
Select Case Loc0
Case 0
Idris_Prelude_46_Classes_46_intToBool = Array(0)
Exit Function
Case Else
Idris_Prelude_46_Classes_46_intToBool = Array(1)
Exit Function
End Select

End Function

Function Idris_io_95_bind(Loc0,Loc1,Loc2,Loc3,Loc4,Loc5)
Loc6 = Idris__123_io_95_bind2_125_(Loc0,Loc1,Loc2,Loc3,Loc4,Loc5)
Loc7 = Idris__123_APPLY0_125_(Loc3,Loc5)
Idris_io_95_bind = Idris__123_APPLY0_125_(Loc6,Loc7)
Exit Function
End Function

Function Idris_io_95_return(Loc0,Loc1,Loc2,Loc3)
Idris_io_95_return = Loc2
Exit Function
End Function

Function Idris_Main_46_main()
Loc0 = 0
Loc1 = 0
Loc2 = Array(65627)
Loc1 = Idris_Prelude_46_show(Loc1,Loc2)
Loc2 = 50
Loc2 = Idris_Main_46_pythag(Loc2)
Loc1 = Idris__123_APPLY0_125_(Loc1,Loc2)
Idris_Main_46_main = Idris_Prelude_46_putStr(Loc0,Loc1)
Exit Function
End Function

Function Idris_mkForeignPrim()
Idris_mkForeignPrim = 0
Exit Function
End Function

Function Idris_Prelude_46_Algebra_46_neutral(Loc0,Loc1)
Select Case Loc1(0)
Case 0
Loc2 = Loc1(1)
Loc3 = Loc1(2)
Idris_Prelude_46_Algebra_46_neutral = Loc3
Exit Function
End Select

End Function

Function Idris_prim_95__95_addInt(Loc0,Loc1)
Idris_prim_95__95_addInt = (Loc0 + Loc1)
Exit Function
End Function

Function Idris_prim_95__95_concat(Loc0,Loc1)
Idris_prim_95__95_concat = Idris_Append(Loc0, Loc1)
Exit Function
End Function

Function Idris_prim_95__95_eqInt(Loc0,Loc1)
Idris_prim_95__95_eqInt = (Loc0 = Loc1)
Exit Function
End Function

Function Idris_prim_95__95_mulInt(Loc0,Loc1)
Idris_prim_95__95_mulInt = (Loc0 * Loc1)
Exit Function
End Function

Function Idris_prim_95__95_sextInt_95_BigInt(Loc0)
Idris_prim_95__95_sextInt_95_BigInt = Loc0
Exit Function
End Function

Function Idris_prim_95__95_sltInt(Loc0,Loc1)
Idris_prim_95__95_sltInt = (Loc0 < Loc1)
Exit Function
End Function

Function Idris_prim_95__95_subInt(Loc0,Loc1)
Idris_prim_95__95_subInt = (Loc0 - Loc1)
Exit Function
End Function

Function Idris_prim_95__95_toStrInt(Loc0)
Idris_prim_95__95_toStrInt = CStr(Loc0)
Exit Function
End Function

Function Idris_prim_95__95_writeString(Loc0,Loc1)
Idris_prim_95__95_writeString = Idris_WriteStr(Loc1)
Exit Function
End Function

Function Idris_prim_95_io_95_bind(Loc0,Loc1,Loc2,Loc3)
Idris_prim_95_io_95_bind = Idris__123_APPLY0_125_(Loc3,Loc2)
Exit Function
End Function

Function Idris_Prelude_46_Applicative_46_pure(Loc0,Loc1,Loc2)
Idris_Prelude_46_Applicative_46_pure = Idris__123_APPLY0_125_(Loc2,Loc1)
Exit Function
End Function

Function Idris_Prelude_46_putStr(Loc0,Loc1)
Loc2 = 0
Loc3 = 0
Loc4 = 0
Loc5 = Array(65641,Loc1)
Loc6 = Array(65642)
Idris_Prelude_46_putStr = Array(65643,Loc2,Loc3,Loc4,Loc5,Loc6)
Exit Function
End Function

Function Idris_Main_46_pythag(Loc0)
Loc1 = 0
Loc2 = 0
Loc3 = 1
Loc3 = Idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(Loc3,Loc0)
Loc4 = Array(65626)
Idris_Main_46_pythag = Idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(Loc1,Loc2,Loc3,Loc4)
Exit Function
End Function

Function Idris_run_95__95_IO(Loc0,Loc1)
Loc2 = 0
Idris_run_95__95_IO = Idris__123_APPLY0_125_(Loc1,Loc2)
Exit Function
End Function

Function Idris_Prelude_46_show(Loc0,Loc1)
Idris_Prelude_46_show = Loc1
Exit Function
End Function

Function Idris_unsafePerformPrimIO()
Idris_unsafePerformPrimIO = 0
Exit Function
End Function

Function Idris_world(Loc0)
Idris_world = Loc0
Exit Function
End Function

Function Idris_Prelude_46_Bool_46__124__124_(Loc0,Loc1)
Select Case Loc0(0)
Case 0

Idris_Prelude_46_Bool_46__124__124_ = Idris__123_EVAL0_125_(Loc1)
Exit Function
Case 1

Idris_Prelude_46_Bool_46__124__124_ = Array(1)
Exit Function
End Select

End Function

Function Idris__123_APPLY0_125_(Loc0,Loc1)
Select Case Loc0(0)
Case 65620

Idris__123_APPLY0_125_ = Idris_Main_46__123_pythag0_125_(Loc1)
Exit Function
Case 65621

Idris__123_APPLY0_125_ = Idris_Main_46__123_pythag1_125_(Loc1)
Exit Function
Case 65622

Idris__123_APPLY0_125_ = Idris_Main_46__123_pythag2_125_(Loc1)
Exit Function
Case 65623
Loc2 = Loc0(1)
Loc3 = Loc0(2)
Loc4 = Loc0(3)
Idris__123_APPLY0_125_ = Idris_Main_46__123_pythag3_125_(Loc2,Loc3,Loc4,Loc1)
Exit Function
Case 65624
Loc2 = Loc0(1)
Loc3 = Loc0(2)
Idris__123_APPLY0_125_ = Idris_Main_46__123_pythag4_125_(Loc2,Loc3,Loc1)
Exit Function
Case 65625
Loc2 = Loc0(1)
Idris__123_APPLY0_125_ = Idris_Main_46__123_pythag5_125_(Loc2,Loc1)
Exit Function
Case 65626

Idris__123_APPLY0_125_ = Idris_Main_46__123_pythag6_125_(Loc1)
Exit Function
Case 65627

Idris__123_APPLY0_125_ = Idris_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c(Loc1)
Exit Function
Case 65628
Loc2 = Loc0(1)
Loc3 = Loc0(2)
Loc4 = Loc0(3)
Loc5 = Loc0(4)
Loc6 = Loc0(5)
Idris__123_APPLY0_125_ = Idris_Prelude_46_Basics_46__46_(Loc2,Loc3,Loc4,Loc5,Loc6,Loc1)
Exit Function
Case 65629
Loc2 = Loc0(1)
Idris__123_APPLY0_125_ = Idris_Prelude_46_Basics_46_id(Loc2,Loc1)
Exit Function
Case 65630
Loc2 = Loc0(1)
Idris__123_APPLY0_125_ = Idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord0_125_(Loc2,Loc1)
Exit Function
Case 65631

Idris__123_APPLY0_125_ = Idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord1_125_(Loc1)
Exit Function
Case 65632
Loc2 = Loc0(1)
Idris__123_APPLY0_125_ = Idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord2_125_(Loc2,Loc1)
Exit Function
Case 65633

Idris__123_APPLY0_125_ = Idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord3_125_(Loc1)
Exit Function
Case 65634
Loc2 = Loc0(1)
Loc3 = Loc0(2)
Idris__123_APPLY0_125_ = Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_0_125_(Loc2,Loc3,Loc1)
Exit Function
Case 65635
Loc2 = Loc0(1)
Idris__123_APPLY0_125_ = Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_1_125_(Loc2,Loc1)
Exit Function
Case 65636

Idris__123_APPLY0_125_ = Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_2_125_(Loc1)
Exit Function
Case 65637

Idris__123_APPLY0_125_ = Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_3_125_(Loc1)
Exit Function
Case 65638

Idris__123_APPLY0_125_ = Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_4_125_(Loc1)
Exit Function
Case 65639
Loc2 = Loc0(1)
Idris__123_APPLY0_125_ = Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_5_125_(Loc2,Loc1)
Exit Function
Case 65640

Idris__123_APPLY0_125_ = Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_6_125_(Loc1)
Exit Function
Case 65641
Loc2 = Loc0(1)
Idris__123_APPLY0_125_ = Idris_Prelude_46__123_putStr0_125_(Loc2,Loc1)
Exit Function
Case 65642

Idris__123_APPLY0_125_ = Idris_Prelude_46__123_putStr1_125_(Loc1)
Exit Function
Case 65643
Loc2 = Loc0(1)
Loc3 = Loc0(2)
Loc4 = Loc0(3)
Loc5 = Loc0(4)
Loc6 = Loc0(5)
Idris__123_APPLY0_125_ = Idris_io_95_bind(Loc2,Loc3,Loc4,Loc5,Loc6,Loc1)
Exit Function
Case 65644
Loc2 = Loc0(1)
Loc3 = Loc0(2)
Loc4 = Loc0(3)
Idris__123_APPLY0_125_ = Idris_io_95_return(Loc2,Loc3,Loc4,Loc1)
Exit Function
Case 65645

Idris__123_APPLY0_125_ = Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c0_125_(Loc1)
Exit Function
Case 65646

Idris__123_APPLY0_125_ = Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c1_125_(Loc1)
Exit Function
Case 65647

Idris__123_APPLY0_125_ = Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c2_125_(Loc1)
Exit Function
Case 65648

Idris__123_APPLY0_125_ = Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c3_125_(Loc1)
Exit Function
Case 65649

Idris__123_APPLY0_125_ = Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c4_125_(Loc1)
Exit Function
Case 65650
Loc2 = Loc0(1)
Loc3 = Loc0(2)
Loc4 = Loc0(3)
Loc5 = Loc0(4)
Loc6 = Loc0(5)
Loc7 = Loc0(6)
Idris__123_APPLY0_125_ = Idris__123_io_95_bind1_125_(Loc2,Loc3,Loc4,Loc5,Loc6,Loc7,Loc1)
Exit Function
Case Else
Idris__123_APPLY0_125_ = 0
Exit Function
End Select

End Function

Function Idris__123_EVAL0_125_(Loc0)
Select Case Loc0
Case Else
Idris__123_EVAL0_125_ = Loc0
Exit Function
End Select

End Function

Function Idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord0_125_(Loc0,Loc1)
Idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord0_125_ = Idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33_compare_58_0(Loc0,Loc1)
Exit Function
End Function

Function Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c0_125_(Loc0)
Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c0_125_ = CStr(Loc0)
Exit Function
End Function

Function Idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61_0_125_(Loc0,Loc1)
Loc2 = (Loc0 = Loc1)
Select Case Loc2
Case 0
Idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61_0_125_ = Array(0)
Exit Function
Case Else
Idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61_0_125_ = Array(1)
Exit Function
End Select

End Function

Function Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_0_125_(Loc0,Loc1,Loc2)
Loc3 = 0
Loc4 = 0
Loc5 = 0
Loc5 = Array(65629,Loc5)
Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_0_125_ = Idris_Prelude_46_List_46_foldrImpl(Loc3,Loc4,Loc0,Loc1,Loc5,Loc2)
Exit Function
End Function

Function Idris__123_io_95_bind0_125_(Loc0,Loc1,Loc2,Loc3,Loc4,Loc5,Loc6)
Idris__123_io_95_bind0_125_ = Idris__123_APPLY0_125_(Loc4,Loc6)
Exit Function
End Function

Function Idris_Prelude_46__123_putStr0_125_(Loc0,Loc1)
Idris_Prelude_46__123_putStr0_125_ = Idris_WriteStr(Loc0)
Exit Function
End Function

Function Idris_Main_46__123_pythag0_125_(Loc0)
Loc1 = Array(0)
Idris_Main_46__123_pythag0_125_ = Array(1,Loc0,Loc1)
Exit Function
End Function

Function Idris__123_runMain0_125_()
Loc0 = Idris_Main_46_main()
Loc1 = 0
Loc0 = Idris__123_APPLY0_125_(Loc0,Loc1)
Idris__123_runMain0_125_ = Idris__123_EVAL0_125_(Loc0)
Exit Function
End Function

Function Idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord1_125_(Loc0)
Idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord1_125_ = Array(65630,Loc0)
Exit Function
End Function

Function Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c1_125_(Loc0)
Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c1_125_ = CStr(Loc0)
Exit Function
End Function

Function Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_1_125_(Loc0,Loc1)
Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_1_125_ = Array(65634,Loc0,Loc1)
Exit Function
End Function

Function Idris__123_io_95_bind1_125_(Loc0,Loc1,Loc2,Loc3,Loc4,Loc5,Loc6)
Loc7 = Idris__123_io_95_bind0_125_(Loc0,Loc1,Loc2,Loc3,Loc4,Loc5,Loc6)
Idris__123_io_95_bind1_125_ = Idris__123_APPLY0_125_(Loc7,Loc5)
Exit Function
End Function

Function Idris_Prelude_46__123_putStr1_125_(Loc0)
Loc1 = 0
Loc2 = 0
Loc3 = Array(0)
Idris_Prelude_46__123_putStr1_125_ = Array(65644,Loc1,Loc2,Loc3)
Exit Function
End Function

Function Idris_Main_46__123_pythag1_125_(Loc0)
Idris_Main_46__123_pythag1_125_ = Array(65620)
Exit Function
End Function

Function Idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord2_125_(Loc0,Loc1)
Loc2 = 0
Loc3 = Idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int()
Loc2 = Idris_Prelude_46_Classes_46_compare(Loc2,Loc3)
Loc2 = Idris__123_APPLY0_125_(Loc2,Loc0)
Loc2 = Idris__123_APPLY0_125_(Loc2,Loc1)
Select Case Loc2(0)
Case 0

Idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord2_125_ = Array(1)
Exit Function
Case Else
Idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord2_125_ = Array(0)
Exit Function
End Select

End Function

Function Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c2_125_(Loc0)
Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c2_125_ = CStr(Loc0)
Exit Function
End Function

Function Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_2_125_(Loc0)
Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_2_125_ = Array(65635,Loc0)
Exit Function
End Function

Function Idris__123_io_95_bind2_125_(Loc0,Loc1,Loc2,Loc3,Loc4,Loc5)
Idris__123_io_95_bind2_125_ = Array(65650,Loc0,Loc1,Loc2,Loc3,Loc4,Loc5)
Exit Function
End Function

Function Idris_Main_46__123_pythag2_125_(Loc0)
Idris_Main_46__123_pythag2_125_ = Array(0)
Exit Function
End Function

Function Idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord3_125_(Loc0)
Idris_Prelude_46_Classes_46__123_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord3_125_ = Array(65632,Loc0)
Exit Function
End Function

Function Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c3_125_(Loc0)
Loc1 = 0
Loc2 = 0
Loc3 = 0
Loc4 = 0
Loc5 = Array(65646)
Loc6 = Array(65647)
Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c3_125_ = Idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(Loc1,Loc2,Loc3,Loc4,Loc5,Loc6,Loc0)
Exit Function
End Function

Function Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_3_125_(Loc0)
Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_3_125_ = Array(65636)
Exit Function
End Function

Function Idris_Main_46__123_pythag3_125_(Loc0,Loc1,Loc2,Loc3)
Loc4 = Array(0,Loc1,Loc2)
Loc4 = Array(0,Loc0,Loc4)
Loc5 = Array(0)
Idris_Main_46__123_pythag3_125_ = Array(1,Loc4,Loc5)
Exit Function
End Function

Function Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c4_125_(Loc0)
Loc1 = 0
Loc2 = 0
Loc3 = 0
Loc4 = 0
Loc5 = Array(65645)
Loc6 = Array(65648)
Idris__123_PE_95_List_32_a_32_instance_32_of_32_Prelude_46_Show_95_f5d3ac2c4_125_ = Idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(Loc1,Loc2,Loc3,Loc4,Loc5,Loc6,Loc0)
Exit Function
End Function

Function Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_4_125_(Loc0)
Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_4_125_ = Array(65637)
Exit Function
End Function

Function Idris_Main_46__123_pythag4_125_(Loc0,Loc1,Loc2)
Loc3 = 0
Loc4 = 0
Loc5 = 0
Loc6 = Array(65621)
Loc7 = Array(65622)
Loc6 = Array(0,Loc6,Loc7)
Loc7 = (Loc2 * Loc2)
Loc8 = (Loc0 * Loc0)
Loc7 = (Loc7 + Loc8)
Loc8 = (Loc1 * Loc1)
Loc7 = (Loc7 = Loc8)
Select Case Loc7
Case 0
Loc7 = Array(0)

Case Else
Loc7 = Array(1)

End Select
Loc5 = Idris_Prelude_46_Applicative_46_guard(Loc5,Loc6,Loc7)
Loc6 = Array(65623,Loc2,Loc0,Loc1)
Idris_Main_46__123_pythag4_125_ = Idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(Loc3,Loc4,Loc5,Loc6)
Exit Function
End Function

Function Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_5_125_(Loc0,Loc1)
Loc2 = 0
Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_5_125_ = Idris_Prelude_46_List_46__43__43_(Loc2,Loc0,Loc1)
Exit Function
End Function

Function Idris_Main_46__123_pythag5_125_(Loc0,Loc1)
Loc2 = 0
Loc3 = 0
Loc4 = 1
Loc4 = Idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(Loc4,Loc1)
Loc5 = Array(65624,Loc1,Loc0)
Idris_Main_46__123_pythag5_125_ = Idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(Loc2,Loc3,Loc4,Loc5)
Exit Function
End Function

Function Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_6_125_(Loc0)
Idris_Prelude_46_Monad_46__123_Prelude_46_List_32_instance_32_of_32_Prelude_46_Monad_46_Monad_44__32_method_32__62__62__61_6_125_ = Array(65639,Loc0)
Exit Function
End Function

Function Idris_Main_46__123_pythag6_125_(Loc0)
Loc1 = 0
Loc2 = 0
Loc3 = 1
Loc3 = Idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(Loc3,Loc0)
Loc4 = Array(65625,Loc0)
Idris_Main_46__123_pythag6_125_ = Idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(Loc1,Loc2,Loc3,Loc4)
Exit Function
End Function

Function Idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Char_58__33_decEq_58_0_58_primitiveNotEq_58_0()
Idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Char_58__33_decEq_58_0_58_primitiveNotEq_58_0 = 0
Exit Function
End Function

Function Idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Int_58__33_decEq_58_0_58_primitiveNotEq_58_0()
Idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Int_58__33_decEq_58_0_58_primitiveNotEq_58_0 = 0
Exit Function
End Function

Function Idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Integer_58__33_decEq_58_0_58_primitiveNotEq_58_0()
Idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_Integer_58__33_decEq_58_0_58_primitiveNotEq_58_0 = 0
Exit Function
End Function

Function Idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_String_58__33_decEq_58_0_58_primitiveNotEq_58_0()
Idris_Decidable_46_Equality_46_Decidable_46_Equality_46__64_Decidable_46_Equality_46_DecEq_36_String_58__33_decEq_58_0_58_primitiveNotEq_58_0 = 0
Exit Function
End Function

Function Idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0(Loc0,Loc1,Loc2,Loc3,Loc4)
Select Case Loc3
Case 0
Idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0 = Array(1,Loc4,Loc2)
Exit Function
Case Else
Loc5 = 1
Loc5 = (Loc3 - Loc5)
Loc6 = 0
Loc7 = 0
Loc8 = Array(1,Loc4,Loc2)
Loc9 = 1
Loc9 = (Loc4 - Loc9)
Idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0 = Idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0(Loc6,Loc7,Loc8,Loc5,Loc9)
Exit Function
End Select

End Function

Function Idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(Loc0,Loc1,Loc2,Loc3,Loc4,Loc5)
Select Case Loc5(0)
Case 1
Loc6 = Loc5(1)
Loc7 = Loc5(2)
Select Case Loc7(0)
Case 0

Loc8 = 0
Loc8 = Idris_Prelude_46_show(Loc8,Loc3)
Loc8 = Idris__123_APPLY0_125_(Loc8,Loc6)
Idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0 = Idris_Append(Loc4, Loc8)
Exit Function
Case Else
Loc8 = 0
Loc9 = 0
Loc10 = 0
Loc11 = 0
Loc11 = Idris_Prelude_46_show(Loc11,Loc3)
Loc11 = Idris__123_APPLY0_125_(Loc11,Loc6)
Loc12 = ", "
Loc11 = Idris_Append(Loc11, Loc12)
Loc11 = Idris_Append(Loc4, Loc11)
Idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0 = Idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(Loc8,Loc9,Loc10,Loc3,Loc11,Loc7)
Exit Function
End Select

Case 0

Idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0 = Loc4
Exit Function
End Select

End Function

Function Idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0(Loc0,Loc1)
Loc2 = Idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__61__58_0(Loc0,Loc1)
Select Case Loc2(0)
Case 0

Idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0 = Array(0)
Exit Function
Case 1

Loc3 = 0
Loc4 = 0
Loc5 = Array(0)
Loc6 = (Loc1 - Loc0)
Loc6 = Loc6
Idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0 = Idris_Prelude_46_Prelude_46__64_Prelude_46_Enum_36_Int_58__33_enumFromTo_58_0_58_go_58_0(Loc3,Loc4,Loc5,Loc6,Loc1)
Exit Function
End Select

End Function

Function Idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0(Loc0,Loc1,Loc2,Loc3)
Loc4 = 0
Loc5 = 0
Loc6 = 0
Loc7 = Array(65638)
Loc8 = Array(65640)
Loc9 = Array(0)
Loc8 = Array(0,Loc8,Loc9)
Loc4 = Idris_Prelude_46_Foldable_46_concatMap(Loc4,Loc5,Loc6,Loc7,Loc8,Loc3)
Idris_Prelude_46_Monad_46_Prelude_46__64_Prelude_46_Monad_46_Monad_36_List_58__33__62__62__61__58_0 = Idris__123_APPLY0_125_(Loc4,Loc2)
Exit Function
End Function

Function Idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__61__58_0(Loc0,Loc1)
Loc2 = 0
Loc3 = Idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int()
Loc2 = Idris_Prelude_46_Classes_46__60_(Loc2,Loc3)
Loc2 = Idris__123_APPLY0_125_(Loc2,Loc0)
Loc2 = Idris__123_APPLY0_125_(Loc2,Loc1)
Select Case Loc2(0)
Case 0

Idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__61__58_0 = Idris_Prelude_46_Classes_46__123_Prelude_46_Classes_46_Int_32_instance_32_of_32_Prelude_46_Classes_46_Ord_44__32_method_32__60__61_0_125_(Loc0,Loc1)
Exit Function
Case 1

Idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__61__58_0 = Array(1)
Exit Function
End Select

End Function

Function Idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33_compare_58_0(Loc0,Loc1)
Loc2 = (Loc0 = Loc1)
Select Case Loc2
Case 0
Loc2 = Array(0)

Case Else
Loc2 = Array(1)

End Select
Select Case Loc2(0)
Case 0

Loc3 = (Loc0 < Loc1)
Select Case Loc3
Case 0
Loc3 = Array(0)

Case Else
Loc3 = Array(1)

End Select
Select Case Loc3(0)
Case 0

Idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33_compare_58_0 = Array(2)
Exit Function
Case 1

Idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33_compare_58_0 = Array(0)
Exit Function
End Select

Case 1

Idris_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33_compare_58_0 = Array(1)
Exit Function
End Select

End Function

Function Idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0(Loc0,Loc1,Loc2,Loc3,Loc4,Loc5,Loc6)
Select Case Loc6(0)
Case 0
Loc7 = Loc6(1)
Loc8 = Loc6(2)
Loc9 = "("
Loc10 = 0
Loc10 = Idris_Prelude_46_show(Loc10,Loc4)
Loc10 = Idris__123_APPLY0_125_(Loc10,Loc7)
Loc11 = ", "
Loc12 = 0
Loc12 = Idris_Prelude_46_show(Loc12,Loc5)
Loc12 = Idris__123_APPLY0_125_(Loc12,Loc8)
Loc13 = ")"
Loc12 = Idris_Append(Loc12, Loc13)
Loc11 = Idris_Append(Loc11, Loc12)
Loc10 = Idris_Append(Loc10, Loc11)
Idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36__40_a_44__32_b_41__58__33_show_58_0 = Idris_Append(Loc9, Loc10)
Exit Function
End Select

End Function

Function Idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0(Loc0,Loc1,Loc2,Loc3)
Loc4 = "["
Loc5 = 0
Loc6 = 0
Loc7 = 0
Loc8 = ""
Loc5 = Idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0_58_show_39__58_0(Loc5,Loc6,Loc7,Loc2,Loc8,Loc3)
Loc6 = "]"
Loc5 = Idris_Append(Loc5, Loc6)
Idris_Prelude_46_Prelude_46__64_Prelude_46_Show_36_List_32_a_58__33_show_58_0 = Idris_Append(Loc4, Loc5)
Exit Function
End Function

Function Idris__95_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__58_0_95_with_95_85(Loc0,Loc1,Loc2)
Select Case Loc0(0)
Case 0

Idris__95_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__58_0_95_with_95_85 = Array(1)
Exit Function
Case Else
Idris__95_Prelude_46_Classes_46_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int_58__33__60__58_0_95_with_95_85 = Array(0)
Exit Function
End Select

End Function

Function Idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int()
Loc0 = Array(65631)
Loc1 = Array(65633)
Idris_Prelude_46_Classes_46__64_Prelude_46_Classes_46_Ord_36_Int = Array(0,Loc0,Loc1)
Exit Function
End Function

Function Idris_Void_95_case()
Idris_Void_95_case = 0
Exit Function
End Function

Function Idris_io_95_bind_95_case(Loc0,Loc1,Loc2,Loc3,Loc4,Loc5,Loc6,Loc7)
Idris_io_95_bind_95_case = Idris__123_APPLY0_125_(Loc7,Loc5)
Exit Function
End Function

Function Idris_Void_95_elim()
Idris_Void_95_elim = 0
Exit Function
End Function


Sub Main
    Call Idris__123_runMain0_125_
End Sub

