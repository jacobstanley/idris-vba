{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- {-# OPTIONS_GHC -w #-}

--module IRTS.CodegenVBA (codegenVBA) where
module IRTS.CodegenVBA where

import           Control.Applicative (Applicative(..), (<$>))
import           Control.Monad.Trans.State.Lazy
import           Data.Map (Map)
import qualified Data.Map as M
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
      cgLibFns :: [String]
    , cgNames  :: Map Name Int
    , cgFresh  :: [Int]
    } deriving (Show)

newtype CG a = CG { unCG :: State CGState a }
    deriving (Functor, Applicative, Monad)

runCG :: CG a -> (a, CGState)
runCG (CG cg) = runState cg (CGState [] M.empty [0..])

declareLibFn :: String -> CG ()
declareLibFn fn = CG $ do
    s <- get
    put s { cgLibFns = fn : cgLibFns s }

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
  where
    ((mainDecl, decls), s) = runCG $ do
        ds <- mapM doCodegen (simpleDecls ci)
        m  <- genMain
        return (m, ds)

    code = "' Foreign Functions\n"
        <> showSep "\n" (cgLibFns s) <> "\n\n"
        <> header <> "\n"
        <> genLoop decls <> "\n"
        <> mainDecl

doCodegen :: (Name, SDecl) -> CG String
doCodegen (n, SFun _ args _ exp) = cgFun n args exp

------------------------------------------------------------------------

genMain :: CG String
genMain = do
    runMain <- lookupLabel (sMN 0 "runMain")
    return $ unlines [
        "' Main Entry Point"
      , "Public Sub Main"
      , "    Idris_MakeOnBits8"
      , "    Idris_MakeOnBits16"
      , "    Idris_MakeOnBits32"
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
   , "' Bit patterns for shift left operations"
   , "Private Idris_OnBits8(0 To 7) As Byte"
   , "Private Idris_OnBits16(0 To 15) As Integer"
   , "Private Idris_OnBits32(0 To 31) As Long"
   , ""
   , "' Runtime Functions"
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
   , "    Idris_OnBits32(j) = v + &H80000000"
   , "End Sub"
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
   , "Private Function Idris_LShr8(ByVal value As Byte, ByVal shift As Integer) As Integer"
   , "    Dim hi As Long"
   , "    If (value And &H80) Then hi = &H40"
   , "    Idris_LShr8 = (value And &H7E) \\ (2 ^ shift)"
   , "    Idris_LShr8 = (Idris_LShr8 Or (hi \\ (2 ^ (shift - 1))))"
   , "End Function"
   , ""
   , "Private Function Idris_LShr16(ByVal value As Integer, ByVal shift As Integer) As Integer"
   , "    Dim hi As Long"
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
   , "Private Function Idris_UTrunc16(ByVal value As Long) As Integer"
   , "    Dim masked As Long"
   , "    masked = value And &HFFFF&"
   , "    If masked < &H8000& Then"
   , "        Idris_UTrunc16 = CInt(masked)"
   , "    Else"
   , "        Idris_UTrunc16 = CInt(masked - &H10000)"
   , "    End If"
   , "End Function"
   , ""
   , "Function Idris_UGt16(ByVal x As Integer, ByVal y As Integer) As Boolean"
   , "    Idris_UGt16 = (x > y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Function Idris_UGe16(ByVal x As Integer, ByVal y As Integer) As Boolean"
   , "    Idris_UGe16 = (x >= y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Function Idris_ULt16(ByVal x As Integer, ByVal y As Integer) As Boolean"
   , "    Idris_ULt16 = (x < y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Function Idris_ULe16(ByVal x As Integer, ByVal y As Integer) As Boolean"
   , "    Idris_ULe16 = (x <= y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Function Idris_UGt32(ByVal x As Long, ByVal y As Long) As Boolean"
   , "    Idris_UGt32 = (x > y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Function Idris_UGe32(ByVal x As Long, ByVal y As Long) As Boolean"
   , "    Idris_UGe32 = (x >= y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Function Idris_ULt32(ByVal x As Long, ByVal y As Long) As Boolean"
   , "    Idris_ULt32 = (x < y) Xor (x < 0) Xor (y < 0)"
   , "End Function"
   , ""
   , "Function Idris_ULe32(ByVal x As Long, ByVal y As Long) As Boolean"
   , "    Idris_ULe32 = (x <= y) Xor (x < 0) Xor (y < 0)"
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
   , "Private Function Idris_Append(xs, ys) As String"
   , "    Idris_Append = CStr(xs) + CStr(ys)"
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
      FCon (UN "VBA_Unit") -> return (exp <> ret "0\n")
      _                    -> return (ret exp)
  where
    call args'
        | '%' `elem` f = return (indexedFFI  f args')
        | '/' `elem` f = nativeFFI f args rt
        | otherwise    = return (standardFFI f args')

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

    declRet     = declType rt
    declArgs    = zipWith declArg argNames (map fst args)
    argNames    = map (\i -> "arg" <> show i) ([1..] :: [Int])
    declArg n t = "ByVal " <> n <> " As " <> declType t

    declType t = case t of
        FCon (UN "VBA_Bool")   -> "Boolean"
        FCon (UN "VBA_Bits8")  -> "Byte"
        FCon (UN "VBA_Bits16") -> "Integer"
        FCon (UN "VBA_Bits32") -> "Long"
        FCon (UN "VBA_Int")    -> "Long"
        FCon (UN "VBA_Float")  -> "Double"
        FCon (UN "VBA_String") -> "String"
        x -> error ("Cannot compile foreign type: " <> show x)

    decl name = "Private Declare "
             <> "Function " <> name <> " "
             <> "Lib \"" <> lib <> "\" "
             <> "Alias \"" <> alias <> "\" "
             <> "(" <> showSep ", " declArgs <> ") "
             <> "As " <> declRet

standardFFI :: String -> [String] -> String
standardFFI code []   = code
standardFFI code args = code <> "(" <> showSep "," args <> ")\n"

indexedFFI :: String -> [String] -> String
indexedFFI code args = T.unpack (JavaScript.ffi code args) <> "\n"

------------------------------------------------------------------------
-- Constants

cgConst :: Const -> String
cgConst x = case x of
    I   i -> "CLng(" <> vbaHex i <> "&)"
    BI  i -> "CLng(" <> vbaHex i <> "&)"
    Fl  f -> "CDbl(" <> show f <> ")"
    Ch  i -> "CInt(" <> vbaHex (ord i) <> ")"
    B8  i -> "CByte(" <> vbaHex i <> ")"
    B16 i -> "CInt(" <> vbaHex i <> ")"
    B32 i -> "CLng(" <> vbaHex i <> ")"
    Str s -> cgString s

    TheWorld       -> "0"
    (AType _)      -> "0"
    StrType        -> "0"
    WorldType      -> "0"
    VoidType       -> "0"

    B64  _ -> error (msg "64-bit integers")
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
pattern INt = ITNative
pattern IBg = ITBig

cgPrim :: PrimFn -> [String] -> String

cgPrim (LZExt I8  I16) [x] = "CInt(" <> x <> " And &HFF&)"
cgPrim (LZExt I8  I32) [x] = "CLng(" <> x <> " And &HFF&)"
cgPrim (LZExt I8  INt) [x] = "CLng(" <> x <> " And &HFF&)"
cgPrim (LZExt I8  IBg) [x] = "CLng(" <> x <> " And &HFF&)"
cgPrim (LZExt I16 I32) [x] = "CLng(" <> x <> " And &HFFFF&)"
cgPrim (LZExt I16 INt) [x] = "CLng(" <> x <> " And &HFFFF&)"
cgPrim (LZExt I16 IBg) [x] = "CLng(" <> x <> " And &HFFFF&)"
cgPrim (LZExt I32 INt) [x] = x
cgPrim (LZExt I32 IBg) [x] = x

cgPrim (LSExt _ I16) [x] = "CInt(" <> x <> ")"
cgPrim (LSExt _ I32) [x] = "CLng(" <> x <> ")"
cgPrim (LSExt _ INt) [x] = "CLng(" <> x <> ")"
cgPrim (LSExt _ IBg) [x] = "CLng(" <> x <> ")"

cgPrim (LIntStr _)   [x] = "CStr(" <> x <> ")"
cgPrim (LStrInt _)   [x] = "CInt(" <> x <> ")"
cgPrim (LIntFloat _) [x] = "CDbl(" <> x <> ")"
cgPrim (LFloatInt _) [x] = "CInt(" <> x <> ")"
cgPrim (LChInt _)    [x] = x
cgPrim (LIntCh _)    [x] = x

cgPrim (LTrunc _ I8)  [x] = "CByte(" <> x <> " And &HFF)"
cgPrim (LTrunc _ I16) [x] = "Idris_UTrunc16(" <> x <> ")"

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

cgPrim (LLSHR I8)  [x,s] = "Idris_LShr8(" <> x <> ", " <> s <> ")"
cgPrim (LLSHR I16) [x,s] = "Idris_LShr16(" <> x <> ", " <> s <> ")"
cgPrim (LLSHR I32) [x,s] = "Idris_LShr32(" <> x <> ", " <> s <> ")"

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
