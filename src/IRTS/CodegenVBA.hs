{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -w #-}

--module IRTS.CodegenVBA (codegenVBA) where
module IRTS.CodegenVBA where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Monad.Trans.State.Lazy
import           Data.List (isPrefixOf, sort)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import           IRTS.CodegenCommon
import qualified IRTS.JavaScript.AST as JavaScript
import           IRTS.Lang
import           IRTS.Simplified
import           Idris.Core.TT hiding (str)
import           Numeric (showHex)
import           Prelude hiding (exp)

import           Data.Monoid ((<>))
import           Data.Char

------------------------------------------------------------------------

data CGState = CGState {
      cgLibFns :: Set String
    , cgNames  :: Map Name Int
    , cgFresh  :: [Int]
    } deriving (Show)

newtype CG a = CG { unCG :: State CGState a }
    deriving (Functor, Applicative, Monad)

runCG :: CG a -> (a, CGState)
runCG (CG cg) = runState cg (CGState S.empty M.empty [0..])

declareLibFn :: String -> CG ()
declareLibFn fn = CG $ do
    s <- get
    put s { cgLibFns = S.insert fn (cgLibFns s) }

lookupLabel :: Name -> CG Int
lookupLabel name = CG $ do
    s <- get
    let names = cgNames s
    case M.lookup name names of
      Just x -> return x
      Nothing -> do
        let x:xs = cgFresh s
        put s { cgNames = M.insert name x names
              , cgFresh = xs }
        return x

------------------------------------------------------------------------

codegenVBA :: CodeGenerator
codegenVBA ci = writeFile (outputFile ci) code
    -- writeFile (outputFile ci) (formatGroups (groupsOfDecls sdecls))
  where
    sdecls = simpleDecls ci

    ((mainDecl, decls), s) = runCG $ do
        ds <- mapM doCodegen sdecls
        m  <- genMain
        return (m, ds)

    code = "' Foreign Functions\n"
        <> showSep "\n" (S.toList (cgLibFns s)) <> "\n\n"
        <> header <> "\n"
        <> genLoop decls <> "\n"
        <> mainDecl

doCodegen :: (Name, SDecl) -> CG String
doCodegen (n, SFun _ args _ exp) = cgFun n args exp

------------------------------------------------------------------------

formatGroups :: [Set Name] -> String
formatGroups ss = showSep "\n" (map formatGroup ss)

formatGroup :: Set Name -> String
formatGroup s = "Group (" <> show (S.size s) <> " functions)\n"
             <> showSep "\n" (sort $ map show (S.toList s))
             <> "\n"

groupsOfDecls :: [(Name, SDecl)] -> [Set Name]
groupsOfDecls ds = mergeAll (map groupsOfDecl ds)
  where
    mergeAll []     = []
    mergeAll (g:gs) = case merge g gs of
        Nothing  -> g : mergeAll gs
        Just gs' -> mergeAll gs'

    merge x []                        = Nothing
    merge x (y:ys) | intersecting x y = Just (S.union x y : ys)
                   | otherwise        = (:) <$> pure y <*> merge x ys

    intersecting x y = not (S.null set)
      where
        set = S.intersection x y
        -- apply0 = S.singleton (sMN 0 "APPLY")

groupsOfDecl :: (Name, SDecl) -> Set Name
groupsOfDecl (name, SFun _ _ _ exp) = S.singleton name `S.union` groupsOfExp exp

groupsOfExp :: SExp -> Set Name
groupsOfExp exp = case exp of
    SApp True name _ -> S.singleton name
    SLet     _ x y   -> groupsOfExp x `S.union` groupsOfExp y
    SUpdate  _ x     -> groupsOfExp x
    SCase  _ _ alts  -> S.unions (map groupsOfAlt alts)
    SChkCase _ alts  -> S.unions (map groupsOfAlt alts)
    _                -> S.empty

groupsOfAlt :: SAlt -> Set Name
groupsOfAlt alt = case alt of
    SConstCase     _ exp -> groupsOfExp exp
    SDefaultCase     exp -> groupsOfExp exp
    SConCase _ _ _ _ exp -> groupsOfExp exp

------------------------------------------------------------------------

genMain :: CG String
genMain = do
    runMain <- lookupLabel (sMN 0 "runMain")
    return $ unlines [
        "' Main Entry Point"
      , "Public Sub Main"
      , "    Call Idris_InitRuntime"
      , "    Call Idris(" <> show runMain <> ")"
      , "End Sub"
      ]

genLoop :: [String] -> String
genLoop decls = unlines $ [
      "' Idris Functions"
    , "Private Function Idris(ByVal fn As Integer)"
    , "Select Case fn"
    , ""
    ] ++ decls ++ [
      "End Select"
    , "End Function"
    ]

header :: String
header = unlines [
     "' Registers"
   , "Private R0 as Variant"
   , "Private R1 as Variant"
   , "Private R2 as Variant"
   , "Private R3 as Variant"
   , "Private R4 as Variant"
   , "Private R5 as Variant"
   , "Private R6 as Variant"
   , "Private R7 as Variant"
   , "Private R8 as Variant"
   , "Private R9 as Variant"
   , ""
   , "' Handle to the current process heap"
   , "Private Idris_ProcessHeap As LongPtr"
   , ""
   , "' Bit patterns for shift left operations"
   , "Private Idris_OnBits8(0 To 7) As Byte"
   , "Private Idris_OnBits16(0 To 15) As Integer"
   , "Private Idris_OnBits32(0 To 31) As Long"
   , "Private Idris_OnBits64(0 To 63) As LongLong"
   , ""
   , "' Runtime Foreign Functions"
   , "Private Declare PtrSafe Function Idris_GetProcessHeap Lib \"kernel32\" Alias \"GetProcessHeap\" () As LongPtr"
   , "Private Declare PtrSafe Function Idris_HeapAlloc Lib \"kernel32\" Alias \"HeapAlloc\" (ByVal hHeap As LongPtr, ByVal dwFlags As Long, ByVal dwBytes As LongPtr) As LongPtr"
   , "Private Declare PtrSafe Function Idris_HeapFree Lib \"kernel32\" Alias \"HeapFree\" (ByVal hHeap As LongPtr, ByVal dwFlags As Long, ByVal lpMem As LongPtr) As Long"
   , "Private Declare PtrSafe Sub Idris_CopyMemory Lib \"kernel32\" Alias \"RtlCopyMemory\" (ByVal dst As LongPtr, ByVal src As LongPtr, ByVal n As LongPtr)"
   , "Private Declare PtrSafe Function Idris_StrLen Lib \"kernel32\" Alias \"lstrlenA\" (ByVal ptr As LongPtr) As Long"
   , ""
   , "Private Sub Idris_InitRuntime()"
   , "    Idris_ProcessHeap = Idris_GetProcessHeap"
   , "    Idris_MakeOnBits8"
   , "    Idris_MakeOnBits16"
   , "    Idris_MakeOnBits32"
   , "    Idris_MakeOnBits64"
   , "End Sub"
   , ""
   , "Private Sub Idris_MakeOnBits8()"
   , "    Dim j As Integer"
   , "    Dim v As Integer"
   , "    For j = 0 To 6"
   , "        v = v + (2 ^ j)"
   , "        Idris_OnBits8(j) = v"
   , "    Next j"
   , "    Idris_OnBits8(j) = v + &H80"
   , "End Sub"
   , ""
   , "Private Sub Idris_MakeOnBits16()"
   , "    Dim j As Integer"
   , "    Dim v As Integer"
   , "    For j = 0 To 14"
   , "        v = v + (2 ^ j)"
   , "        Idris_OnBits16(j) = v"
   , "    Next j"
   , "    Idris_OnBits16(j) = v + &H8000"
   , "End Sub"
   , ""
   , "Private Sub Idris_MakeOnBits32()"
   , "    Dim j As Integer"
   , "    Dim v As Long"
   , "    For j = 0 To 30"
   , "        v = v + (2 ^ j)"
   , "        Idris_OnBits32(j) = v"
   , "    Next j"
   , "    Idris_OnBits32(j) = v + &H80000000&"
   , "End Sub"
   , ""
   , "Private Sub Idris_MakeOnBits64()"
   , "    Dim j As Integer"
   , "    Dim v As LongLong"
   , "    Dim p As LongLong"
   , "    For j = 0 To 62"
   , "        p = 2 ^ j"
   , "        v = v + p"
   , "        Idris_OnBits64(j) = v"
   , "    Next j"
   , "    Idris_OnBits64(j) = v + &H8000000000000000^"
   , "End Sub"
   , ""
   , "Private Function Idris_Alloc(ByVal nbytes As LongPtr) As LongPtr"
   , "    Idris_Alloc = Idris_HeapAlloc(Idris_ProcessHeap, 8, nbytes)"
   , "End Function"
   , ""
   , "Private Function Idris_Free(ByVal ptr As LongPtr) As Boolean"
   , "    Idris_Free = Idris_HeapFree(Idris_ProcessHeap, 0, ptr) <> 0"
   , "End Function"
   , ""
   , "Private Function Idris_Shl8(ByVal value As Byte, ByVal shift As Integer) As Byte"
   , "    If (value And (2 ^ (7 - shift))) Then GoTo Overflow"
   , "    Idris_Shl8 = ((value And Idris_OnBits8(7 - shift)) * (2 ^ shift))"
   , "    Exit Function"
   , "Overflow:"
   , "    Idris_Shl8 = ((value And Idris_OnBits8(7 - (shift + 1))) * (2 ^ (shift))) Or &H80"
   , "End Function"
   , ""
   , "Private Function Idris_Shl16(ByVal value As Integer, ByVal shift As Integer) As Integer"
   , "    If (value And (2 ^ (15 - shift))) Then GoTo Overflow"
   , "    Idris_Shl16 = ((value And Idris_OnBits16(15 - shift)) * (2 ^ shift))"
   , "    Exit Function"
   , "Overflow:"
   , "    Idris_Shl16 = ((value And Idris_OnBits16(15 - (shift + 1))) * (2 ^ (shift))) Or &H8000"
   , "End Function"
   , ""
   , "Private Function Idris_Shl32(ByVal value As Long, ByVal shift As Integer) As Long"
   , "    If (value And (2 ^ (31 - shift))) Then GoTo Overflow"
   , "    Idris_Shl32 = ((value And Idris_OnBits32(31 - shift)) * (2 ^ shift))"
   , "    Exit Function"
   , "Overflow:"
   , "    Idris_Shl32 = ((value And Idris_OnBits32(31 - (shift + 1))) * (2 ^ (shift))) Or &H80000000"
   , "End Function"
   , ""
   , "Private Function Idris_Shl64(ByVal value As LongLong, ByVal shift As Integer) As LongLong"
   , "    If (value And (2 ^ (63 - shift))) Then GoTo Overflow"
   , "    Idris_Shl64 = ((value And Idris_OnBits64(63 - shift)) * (2 ^ shift))"
   , "    Exit Function"
   , "Overflow:"
   , "    Idris_Shl64 = ((value And Idris_OnBits64(31 - (shift + 1))) * (2 ^ (shift))) Or &H8000000000000000^"
   , "End Function"
   , ""
   , "Private Function Idris_LShr8(ByVal value As Byte, ByVal shift As Integer) As Byte"
   , "    Dim hi As Byte"
   , "    If (value And &H80) Then hi = &H40"
   , "    Idris_LShr8 = (value And &H7E) \\ (2 ^ shift)"
   , "    Idris_LShr8 = (Idris_LShr8 Or (hi \\ (2 ^ (shift - 1))))"
   , "End Function"
   , ""
   , "Private Function Idris_LShr16(ByVal value As Integer, ByVal shift As Integer) As Integer"
   , "    Dim hi As Integer"
   , "    If (value And &H8000) Then hi = &H4000"
   , "    Idris_LShr16 = (value And &H7FFE) \\ (2 ^ shift)"
   , "    Idris_LShr16 = (Idris_LShr16 Or (hi \\ (2 ^ (shift - 1))))"
   , "End Function"
   , ""
   , "Private Function Idris_LShr32(ByVal value As Long, ByVal shift As Integer) As Long"
   , "    Dim hi As Long"
   , "    If (value And &H80000000) Then hi = &H40000000"
   , "    Idris_LShr32 = (value And &H7FFFFFFE) \\ (2 ^ shift)"
   , "    Idris_LShr32 = (Idris_LShr32 Or (hi \\ (2 ^ (shift - 1))))"
   , "End Function"
   , ""
   , "Private Function Idris_LShr64(ByVal value As LongLong, ByVal shift As Integer) As LongLong"
   , "    Dim hi As LongLong"
   , "    If (value And &H8000000000000000^) Then hi = &H4000000000000000^"
   , "    Idris_LShr64 = (value And &H7FFFFFFFFFFFFFFE^) \\ (2 ^ shift)"
   , "    Idris_LShr64 = (Idris_LShr64 Or (hi \\ (2 ^ (shift - 1))))"
   , "End Function"
   , ""
   , "Private Function Idris_UTrunc_32_16(ByVal value As Long) As Integer"
   , "    Dim masked As Long"
   , "    masked = value And &HFFFF&"
   , "    If masked < &H8000& Then"
   , "        Idris_UTrunc_32_16 = CInt(masked)"
   , "    Else"
   , "        Idris_UTrunc_32_16 = CInt(masked - &H10000)"
   , "    End If"
   , "End Function"
   , ""
   , "Private Function Idris_UTrunc_64_16(ByVal value As LongLong) As Integer"
   , "    Dim masked As LongLong"
   , "    masked = value And &HFFFF^"
   , "    If masked < &H8000^ Then"
   , "        Idris_UTrunc_64_16 = CInt(masked)"
   , "    Else"
   , "        Idris_UTrunc_64_16 = CInt(masked - &H10000^)"
   , "    End If"
   , "End Function"
   , ""
   , "Private Function Idris_UTrunc_64_32(ByVal value As LongLong) As Long"
   , "    Dim masked As LongLong"
   , "    masked = value And &HFFFFFFFF^"
   , "    If masked < &H80000000^ Then"
   , "        Idris_UTrunc_64_32 = CLng(masked)"
   , "    Else"
   , "        Idris_UTrunc_64_32 = CLng(masked - &H100000000^)"
   , "    End If"
   , "End Function"
   , ""
   , "Private Function Idris_UGt16(ByVal x As Integer, ByVal y As Integer) As Boolean"
   , "    Idris_UGt16 = (x > y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Private Function Idris_UGe16(ByVal x As Integer, ByVal y As Integer) As Boolean"
   , "    Idris_UGe16 = (x >= y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Private Function Idris_ULt16(ByVal x As Integer, ByVal y As Integer) As Boolean"
   , "    Idris_ULt16 = (x < y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Private Function Idris_ULe16(ByVal x As Integer, ByVal y As Integer) As Boolean"
   , "    Idris_ULe16 = (x <= y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Private Function Idris_UGt32(ByVal x As Long, ByVal y As Long) As Boolean"
   , "    Idris_UGt32 = (x > y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Private Function Idris_UGe32(ByVal x As Long, ByVal y As Long) As Boolean"
   , "    Idris_UGe32 = (x >= y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Private Function Idris_ULt32(ByVal x As Long, ByVal y As Long) As Boolean"
   , "    Idris_ULt32 = (x < y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Private Function Idris_ULe32(ByVal x As Long, ByVal y As Long) As Boolean"
   , "    Idris_ULe32 = (x <= y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Private Function Idris_UGt64(ByVal x As LongLong, ByVal y As LongLong) As Boolean"
   , "    Idris_UGt64 = (x > y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Private Function Idris_UGe64(ByVal x As LongLong, ByVal y As LongLong) As Boolean"
   , "    Idris_UGe64 = (x >= y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Private Function Idris_ULt64(ByVal x As LongLong, ByVal y As LongLong) As Boolean"
   , "    Idris_ULt64 = (x < y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Private Function Idris_ULe64(ByVal x As LongLong, ByVal y As LongLong) As Boolean"
   , "    Idris_ULe64 = (x <= y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Private Function Idris_PeekBits8(ByVal ptr As LongPtr) As LongPtr"
   , "    Dim x As Byte"
   , "    Call Idris_CopyMemory(VarPtr(x), ptr, Len(x))"
   , "    Idris_PeekBits8 = x"
   , "End Function"
   , ""
   , "Private Function Idris_PeekBits16(ByVal ptr As LongPtr) As Integer"
   , "    Dim x As Integer"
   , "    Call Idris_CopyMemory(VarPtr(x), ptr, Len(x))"
   , "    Idris_PeekBits16 = x"
   , "End Function"
   , ""
   , "Private Function Idris_PeekBits32(ByVal ptr As LongPtr) As Long"
   , "    Dim x As Long"
   , "    Call Idris_CopyMemory(VarPtr(x), ptr, Len(x))"
   , "    Idris_PeekBits32 = x"
   , "End Function"
   , ""
   , "Private Function Idris_PeekBits64(ByVal ptr As LongPtr) As LongLong"
   , "    Dim x As LongLong"
   , "    Call Idris_CopyMemory(VarPtr(x), ptr, Len(x))"
   , "    Idris_PeekBits64 = x"
   , "End Function"
   , ""
   , "Private Function Idris_PeekCString(ByVal ptr As LongPtr) As String"
   , "    Dim slen As Long"
   , "    Dim str() As Byte"
   , "    slen = Idris_StrLen(ptr)"
   , "    ReDim str(0 To slen - 1)"
   , "    Call Idris_CopyMemory(VarPtr(str(0)), ptr, slen)"
   , "    Idris_PeekCString = StrConv(str, vbUnicode)"
   , "End Function"
   , ""
   , "Private Sub Idris_PokeBits8(ByVal ptr As LongPtr, ByVal x As Byte)"
   , "    Call Idris_CopyMemory(ptr, VarPtr(x), Len(x))"
   , "End Sub"
   , ""
   , "Private Sub Idris_PokeBits16(ByVal ptr As LongPtr, ByVal x As Integer)"
   , "    Call Idris_CopyMemory(ptr, VarPtr(x), Len(x))"
   , "End Sub"
   , ""
   , "Private Sub Idris_PokeBits32(ByVal ptr As LongPtr, ByVal x As Long)"
   , "    Call Idris_CopyMemory(ptr, VarPtr(x), Len(x))"
   , "End Sub"
   , ""
   , "Private Sub Idris_PokeBits64(ByVal ptr As LongPtr, ByVal x As LongLong)"
   , "    Call Idris_CopyMemory(ptr, VarPtr(x), Len(x))"
   , "End Sub"
   , ""
   , "Private Function Idris_PlusPtr(ByVal ptr As LongPtr, ByVal off As LongPtr) As LongPtr"
   , "    Idris_PlusPtr = ptr + off"
   , "End Function"
   , ""
   , "Private Function Idris_Error(msg)"
   , "    Call Err.Raise(10101, \"Idris\", msg)"
   , "End Function"
   , ""
   , "Private Function Idris_WriteStr(str)"
   , "    Debug.Print str"
   , "End Function"
   , ""
   , "Private Function Idris_ReadStr() As String"
   , "    Idris_ReadStr = InputBox(\"Input:\", \"Idris\")"
   , "End Function"
   , ""
   , "Private Function Idris_Append(ByVal xs As String, ByVal ys As String) As String"
   , "    Idris_Append = xs + ys"
   , "End Function"
   ]

------------------------------------------------------------------------
-- Variables

cgVar :: LVar -> CG String
cgVar (Loc i)  = return (local i)
cgVar (Glob n) = global <$> lookupLabel n

global :: Int -> String
global i = "G" <> show i

local :: Int -> String
local i = "L" <> show i

register :: Int -> String
register i = "R" <> show i

------------------------------------------------------------------------
-- Functions

cgFun :: Name -> [Name] -> SExp -> CG String
cgFun n args exp = do
    name <- lookupLabel n
    body <- cgExp ret exp
    return $ "' " <> show n <> "\n"
          <> "Case " <> show name <> "\n"
          <> "Idris" <> show name <> ":\n"
          <> concat (take nargs assignments)
          <> body
  where
    ret rexp = "Idris = " <> rexp

    nargs = length args

    assignments = zipWith cgAssign cgLocals cgRegisters

cgAssign :: String -> String -> String
cgAssign lv rv = lv <> " = " <> rv <> "\n"

cgLocals :: [String]
cgLocals = map local [0..]

cgRegisters :: [String]
cgRegisters = map register [0..]

------------------------------------------------------------------------
-- Expressions

cgExp :: (String -> String) -> SExp -> CG String
cgExp ret (SV (Loc i)) =
    return $ ret $ local i <> "\n"

cgExp ret (SV (Glob n)) = do
    label <- lookupLabel n
    return $ ret ("Idris(" <> show label <> ")\n")

cgExp _ (SApp True fn args) = do
    label   <- lookupLabel fn
    appArgs <- mapM cgVar args
    return $ concat (zipWith cgAssign cgRegisters appArgs)
          <> "GoTo Idris" <> show label <> "\n"

cgExp ret (SApp False fn args) = do
    label   <- lookupLabel fn
    appArgs <- mapM cgVar args
    return $ concat (zipWith cgAssign cgRegisters appArgs)
          <> ret ("Idris(" <> show label <> ")\n")

cgExp ret (SLet (Loc i) v sc) = do
    blet <- cgExp (\exp -> local i <> " = " <> exp) v
    bret <- cgExp ret sc
    return (blet <> bret)

cgExp ret (SUpdate _ e) =
    cgExp ret e

cgExp ret (SProj exp i) = do
    v <- cgVar exp
    return $ ret (v <> "(" <> show (i + 1) <> ")\n")

cgExp ret (SCon _ t _ args) = do
    ctorArgs <- mapM cgVar args
    return $ ret $ "Array(" <> showSep "," (show t : ctorArgs) <> ")\n"

cgExp ret (SCase _ e alts) = do
    scr   <- cgVar e
    alts' <- mapM (cgAlt ret scr) alts
    return $ "Select Case " <> scr <> suffix <> "\n"
          <> concat alts'
          <> "End Select\n"
  where
    suffix | any conCase alts = "(0)"
           | otherwise        = ""

    conCase (SConCase _ _ _ _ _) = True
    conCase _                    = False

cgExp ret (SChkCase exp alts) = do
    scr   <- cgVar exp
    alts' <- mapM (cgAlt ret scr) alts
    return $ "Select Case " <> scr <> suffix <> "\n"
          <> concat alts'
          <> "End Select\n"
  where
    suffix | any conCase alts = "(0)"
           | otherwise        = ""

    conCase (SConCase _ _ _ _ _) = True
    conCase _                    = False

cgExp ret (SOp op args) = do
    opArgs <- mapM cgVar args
    return $ ret (cgPrim op opArgs <> "\n")

cgExp ret (SConst c)    = return $ ret $ cgConst c <> "\n"
cgExp ret SNothing      = return $ ret "0\n"
cgExp ret (SError x)    = return $ ret $ "Idris_Error(" <> show x <> ")\n"

cgExp ret (SForeign rt (FStr f) args) = do
    callArgs <- mapM (cgVar . snd) args
    exp <- call callArgs
    case rt of
      FCon (UN "VBA_Unit") | isIndexed -> return (exp <> ret "0\n")
                           | otherwise -> return ("Call " <> exp <> ret "0\n")
      _                                -> return (ret exp)
  where
    call args' | isPrimitive = return (primitiveFFI f args')
               | isIndexed   = return (indexedFFI  f args')
               | isNative    = nativeFFI f args rt
               | otherwise   = return (standardFFI f args')

    isPrimitive = "prim$" `isPrefixOf` f
    isIndexed   = '%' `elem` f
    isNative    = '/' `elem` f

cgExp _ exp = error $ "SExp (" <> show exp <> ") not implemented"

------------------------------------------------------------------------
-- Pattern Matching

cgAlt :: (String -> String) -> String -> SAlt -> CG String
cgAlt ret _   (SConstCase t exp) = do
    body <- cgExp ret exp
    return $ "Case " <> cgConst t <> "\n" <> body

cgAlt ret _   (SDefaultCase exp) = do
    body <- cgExp ret exp
    return $ "Case Else\n" <> body

cgAlt ret scr (SConCase lv t _ args exp) = do
    body <- cgExp ret exp
    return $ "Case " <> show t <> "\n"
                     <> project 1 lv args
                     <> body
  where
    project :: Int -> Int -> [a] -> [Char]
    project _ _ []       = ""
    project i v [_]      = project' i v
    project i v (_ : ns) = project' i v <> project (i + 1) (v + 1) ns

    project' i v = local v <> " = " <> scr <> "(" <> show i <> ")\n"

------------------------------------------------------------------------
-- FFI Styles

nativeFFI :: String -> [(FDesc, LVar)] -> FDesc -> CG String
nativeFFI sig args rt = do
    label <- lookupLabel (sUN sig)
    let name = "FFI" <> show label
    declareLibFn (decl name)
    callArgs <- mapM (cgVar . snd) args
    return (name <> "(" <> showSep "," callArgs <> ")\n")
  where
    (lib, sig') = break (== '/') sig
    alias       = drop 1 sig'

    declArgs    = zipWith declArg argNames (map fst args)
    argNames    = map (\i -> "arg" <> show i) ([1..] :: [Int])
    declArg n t = "ByVal " <> n <> " As " <> declType t

    (funSub, declRet) = case rt of
        FCon (UN "VBA_Unit") -> ("Sub", "")
        _                    -> ("Function", " As " <> declType rt)

    declType t = case t of
        FCon (UN "VBA_Bool")   -> "Boolean"
        FCon (UN "VBA_Bits8")  -> "Byte"
        FCon (UN "VBA_Bits16") -> "Integer"
        FCon (UN "VBA_Bits32") -> "Long"
        FCon (UN "VBA_Bits64") -> "LongLong"
        FCon (UN "VBA_Int")    -> "Long"
        FCon (UN "VBA_Float")  -> "Double"
        FCon (UN "VBA_String") -> "String"
        FCon (UN "VBA_Ptr")    -> "LongPtr"
        x -> error $ "Cannot compile foreign type: " <> show x
                  <> " (in " <> show sig <> ")"

    decl name = "Private Declare PtrSafe "
             <> funSub <> " " <> name <> " "
             <> "Lib \"" <> lib <> "\" "
             <> "Alias \"" <> alias <> "\" "
             <> "(" <> showSep ", " declArgs <> ")"
             <> declRet

standardFFI :: String -> [String] -> String
standardFFI code []   = code
standardFFI code args = code <> "(" <> showSep "," args <> ")\n"

indexedFFI :: String -> [String] -> String
indexedFFI code args = T.unpack (JavaScript.ffi code args) <> "\n"

primitiveFFI :: String -> [String] -> String
primitiveFFI = go
  where
    go "prim$alloc"       [sz]      = "Idris_Alloc(" <> sz <> ")\n"
    go "prim$free"        [ptr]     = "Idris_Free(" <> ptr <> ")\n"
    go "prim$peekBits8"   [ptr]     = "Idris_PeekBits8(" <> ptr <> ")\n"
    go "prim$peekBits16"  [ptr]     = "Idris_PeekBits16(" <> ptr <> ")\n"
    go "prim$peekBits32"  [ptr]     = "Idris_PeekBits32(" <> ptr <> ")\n"
    go "prim$peekBits64"  [ptr]     = "Idris_PeekBits64(" <> ptr <> ")\n"
    go "prim$peekCString" [ptr]     = "Idris_PeekCString(" <> ptr <> ")\n"
    go "prim$pokeBits8"   [ptr,x]   = "Idris_PokeBits8(" <> ptr <> "," <> x <> ")\n"
    go "prim$pokeBits16"  [ptr,x]   = "Idris_PokeBits16(" <> ptr <> "," <> x <> ")\n"
    go "prim$pokeBits32"  [ptr,x]   = "Idris_PokeBits32(" <> ptr <> "," <> x <> ")\n"
    go "prim$pokeBits64"  [ptr,x]   = "Idris_PokeBits64(" <> ptr <> "," <> x <> ")\n"
    go "prim$plusPtr"     [ptr,off] = "Idris_PlusPtr(" <> ptr <> "," <> off <> ")\n"

    go fn args = error $ "Unknown primitive: " <> show fn
                      <> "(" <> show (length args) <> ")"

------------------------------------------------------------------------
-- Constants

cgConst :: Const -> String
cgConst x = case x of
    I   i -> "CLng(" <> vbaHex i <> "&)"
    BI  i -> "CLngLng(" <> vbaHex i <> "^)"
    Fl  f -> "CDbl(" <> show f <> ")"
    Ch  i -> "CInt(" <> vbaHex (ord i) <> ")"
    B8  i -> "CByte(" <> vbaHex i <> ")"
    B16 i -> "CInt(" <> vbaHex i <> ")"
    B32 i -> "CLng(" <> vbaHex i <> "&)"
    B64 i -> "CLngLng(" <> vbaHex i <> "^)"
    Str s -> cgString s

    TheWorld       -> "CLng(42424242)"
    (AType _)      -> "CLng(42424242)"
    StrType        -> "CLng(42424242)"
    ManagedPtrType -> "CLng(42424242)"
    BufferType     -> "CLng(42424242)"
    WorldType      -> "CLng(42424242)"
    PtrType        -> "CLng(42424242)"
    VoidType       -> "CLng(42424242)"

    B64  _ -> error (msg "64-bit integers")
    B8V  _ -> error (msg "8-bit vectors")
    B16V _ -> error (msg "16-bit vectors")
    B32V _ -> error (msg "32-bit vectors")
    B64V _ -> error (msg "64-bit vectors")
    Forgot -> error "Tried to compile: Forgot"
  where
    msg name = name <> " not supported (tried to compile: " <> show x <> ")"

    vbaHex n = "&H" <> showHex n ""

cgString :: String -> String
cgString [] = "\"\""
cgString s  = drop 3 (esc s)
  where
    esc []                    = []
    esc xs@(y:ys) | escape y  = " & ChrW(" <> show (ord y) <> ")" <> esc ys
                  | otherwise = " & \"" <> str xs

    str []                    = "\""
    str xs@(y:ys) | escape y  = "\"" <> esc xs
                  | otherwise = y : str ys

    escape x = x < ' '
            || x > '~'

------------------------------------------------------------------------
-- Primitive Operations

pattern I8  = ITFixed IT8
pattern I16 = ITFixed IT16
pattern I32 = ITFixed IT32
pattern I64 = ITFixed IT64
pattern INt = ITNative
pattern IBg = ITBig

cgPrim :: PrimFn -> [String] -> String

cgPrim (LZExt I8  I16) [x] = "CInt("    <> x <> " And &HFF^)"
cgPrim (LZExt I8  I32) [x] = "CLng("    <> x <> " And &HFF^)"
cgPrim (LZExt I8  I64) [x] = "CLngLng(" <> x <> " And &HFF^)"
cgPrim (LZExt I8  INt) [x] = "CLng("    <> x <> " And &HFF^)"
cgPrim (LZExt I8  IBg) [x] = "CLngLng(" <> x <> " And &HFF^)"

cgPrim (LZExt I16 I32) [x] = "CLng("    <> x <> " And &HFFFF^)"
cgPrim (LZExt I16 I64) [x] = "CLngLng(" <> x <> " And &HFFFF^)"
cgPrim (LZExt I16 INt) [x] = "CLng("    <> x <> " And &HFFFF^)"
cgPrim (LZExt I16 IBg) [x] = "CLngLng(" <> x <> " And &HFFFF^)"

cgPrim (LZExt I32 I64) [x] = "CLngLng(" <> x <> " And &HFFFFFFFF^)"
cgPrim (LZExt I32 INt) [x] = x
cgPrim (LZExt I32 IBg) [x] = "CLngLng(" <> x <> " And &HFFFFFFFF^)"

cgPrim (LZExt I64 IBg) [x] = x

cgPrim (LSExt _ I16) [x] = "CInt("    <> x <> ")"
cgPrim (LSExt _ I32) [x] = "CLng("    <> x <> ")"
cgPrim (LSExt _ I64) [x] = "CLngLng(" <> x <> ")"
cgPrim (LSExt _ INt) [x] = "CLng("    <> x <> ")"
cgPrim (LSExt _ IBg) [x] = "CLngLng(" <> x <> ")"

cgPrim (LIntStr _)   [x] = "CStr(" <> x <> ")"
cgPrim (LStrInt _)   [x] = "CLng(" <> x <> ")"
cgPrim (LIntFloat _) [x] = "CDbl(" <> x <> ")"
cgPrim (LFloatInt _) [x] = "CLng(" <> x <> ")"
cgPrim (LChInt _)    [x] = "CLng(" <> x <> ")"
cgPrim (LIntCh _)    [x] = "CInt(" <> x <> ")"

cgPrim (LTrunc _   I8)  [x] = "CByte(" <> x <> " And &HFF)"

cgPrim (LTrunc I32 I16) [x] = "Idris_UTrunc_32_16(" <> x <> ")"
cgPrim (LTrunc I64 I16) [x] = "Idris_UTrunc_64_16(" <> x <> ")"
cgPrim (LTrunc INt I16) [x] = "Idris_UTrunc_32_16(" <> x <> ")"
cgPrim (LTrunc IBg I16) [x] = "Idris_UTrunc_64_16(" <> x <> ")"

cgPrim (LTrunc I64 I32) [x] = "Idris_UTrunc_64_32(" <> x <> ")"
cgPrim (LTrunc INt I32) [x] = x
cgPrim (LTrunc IBg I32) [x] = "Idris_UTrunc_64_32(" <> x <> ")"

cgPrim (LTrunc INt I64) [x] = "CLngLng(" <> x <> ")"
cgPrim (LTrunc IBg I64) [x] = x

cgPrim p@(LSExt  _ _) _ = error $ "Invalid conversion: " <> show p
cgPrim p@(LZExt  _ _) _ = error $ "Invalid conversion: " <> show p
cgPrim p@(LTrunc _ _) _ = error $ "Invalid conversion: " <> show p

cgPrim (LPlus  _) [l,r] = "(" <> l <> " + "   <> r <> ")"
cgPrim (LMinus _) [l,r] = "(" <> l <> " - "   <> r <> ")"
cgPrim (LTimes _) [l,r] = "(" <> l <> " * "   <> r <> ")"
--cgPrim (LSDiv  _) [l,r] = "(" <> l <> " \\ "  <> r <> ")"
cgPrim (LSRem  _) [l,r] = "(" <> l <> " Mod " <> r <> ")"

cgPrim (LSHL I8)  [x,s] = "Idris_Shl8(" <> x <> ", " <> s <> ")"
cgPrim (LSHL I16) [x,s] = "Idris_Shl16(" <> x <> ", " <> s <> ")"
cgPrim (LSHL I32) [x,s] = "Idris_Shl32(" <> x <> ", " <> s <> ")"
cgPrim (LSHL I64) [x,s] = "Idris_Shl64(" <> x <> ", " <> s <> ")"

cgPrim (LLSHR I8)  [x,s] = "Idris_LShr8(" <> x <> ", " <> s <> ")"
cgPrim (LLSHR I16) [x,s] = "Idris_LShr16(" <> x <> ", " <> s <> ")"
cgPrim (LLSHR I32) [x,s] = "Idris_LShr32(" <> x <> ", " <> s <> ")"
cgPrim (LLSHR I64) [x,s] = "Idris_LShr64(" <> x <> ", " <> s <> ")"

cgPrim (LEq  _) [l,r] = "(" <> l <> " = "  <> r <> ")"

cgPrim (LLt I8) [l,r] = "(" <> l <> " < " <> r <> ")"
cgPrim (LLe I8) [l,r] = "(" <> l <> " <= " <> r <> ")"
cgPrim (LGt I8) [l,r] = "(" <> l <> " > " <> r <> ")"
cgPrim (LGe I8) [l,r] = "(" <> l <> " >= " <> r <> ")"

cgPrim (LLt I16) [l,r] = "Idris_ULt16(" <> l <> "," <> r <> ")"
cgPrim (LLe I16) [l,r] = "Idris_ULe16(" <> l <> "," <> r <> ")"
cgPrim (LGt I16) [l,r] = "Idris_UGt16(" <> l <> "," <> r <> ")"
cgPrim (LGe I16) [l,r] = "Idris_UGe16(" <> l <> "," <> r <> ")"

cgPrim (LLt I32) [l,r] = "Idris_ULt32(" <> l <> "," <> r <> ")"
cgPrim (LLe I32) [l,r] = "Idris_ULe32(" <> l <> "," <> r <> ")"
cgPrim (LGt I32) [l,r] = "Idris_UGt32(" <> l <> "," <> r <> ")"
cgPrim (LGe I32) [l,r] = "Idris_UGe32(" <> l <> "," <> r <> ")"

cgPrim (LLt I64) [l,r] = "Idris_ULt64(" <> l <> "," <> r <> ")"
cgPrim (LLe I64) [l,r] = "Idris_ULe64(" <> l <> "," <> r <> ")"
cgPrim (LGt I64) [l,r] = "Idris_UGt64(" <> l <> "," <> r <> ")"
cgPrim (LGe I64) [l,r] = "Idris_UGe64(" <> l <> "," <> r <> ")"

cgPrim (LSLt _) [l,r] = "(" <> l <> " < "  <> r <> ")"
cgPrim (LSLe _) [l,r] = "(" <> l <> " <= " <> r <> ")"
cgPrim (LSGt _) [l,r] = "(" <> l <> " > "  <> r <> ")"
cgPrim (LSGe _) [l,r] = "(" <> l <> " >= " <> r <> ")"

cgPrim (LAnd   _) [l,r] = "(" <> l <> " And "  <> r <> ")"
cgPrim (LOr    _) [l,r] = "(" <> l <> " Or  "  <> r <> ")"
cgPrim (LXOr   _) [l,r] = "(" <> l <> " Xor "  <> r <> ")"
cgPrim (LCompl _) [x]   = "(Not " <> x <> ")"

cgPrim LStrEq [l,r] = "(" <> l <> " = " <> r <> ")"

cgPrim LStrConcat [l,r] = "Idris_Append(" <> l <> ", " <> r <> ")"
cgPrim LStrCons   [l,r] = "Idris_Append(Chr(" <> l <> "), " <> r <> ")"
cgPrim LStrHead   [x]   = "Asc(Left(" <> x <> ", 1))"
cgPrim LStrTail   [x]   = "Mid(" <> x <> ", 2)"
cgPrim LStrIndex  [x,i] = "Asc(Mid(" <> x <> ", (" <> i <> "+1), 1))"
cgPrim LStrLen    [x]   = "Len(" <> x <> ")"
cgPrim LStrRev    [x]   = "StrReverse(" <> x <> ")"

cgPrim LWriteStr [_,str] = "Idris_WriteStr(" <> str <> ")"
cgPrim LReadStr  [_]     = "Idris_ReadStr()"

cgPrim fn args = error $ "Primitive not implemented ("
                      <> "prim = " <> show fn <> ", "
                      <> "args = " <> show args <> ")"
