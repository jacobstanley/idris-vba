{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# OPTIONS_GHC -w #-}

--module IRTS.CodegenVBA (codegenVBA) where
module IRTS.CodegenVBA where

import           Control.Applicative (Applicative(..))
import           Control.Monad.Trans.State.Lazy
import           Data.List (sort)
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Tuple (swap)
import           IRTS.CodegenCommon
import qualified IRTS.JavaScript.AST as JavaScript
import           IRTS.Lang
import           IRTS.Simplified
import           Idris.Core.TT hiding (str)
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

assignName :: Name -> CG String
assignName name = CG $ do
    s <- get
    let names = cgNames s
    case M.lookup name names of
      Just x -> return (global x)
      Nothing -> do
        let x:xs = cgFresh s
        put s { cgNames = M.insert name x names
              , cgFresh = xs }
        return (global x)

------------------------------------------------------------------------

codegenVBA :: CodeGenerator
codegenVBA ci = writeFile (outputFile ci) code
  where
    ((mainDecl, decls), s) = runCG $ do
        ds <- mapM doCodegen (simpleDecls ci)
        m  <- genMain
        return (m, ds)

    code = nameMappings s <> "\n"
        <> showSep "\n" (cgLibFns s) <> "\n"
        <> header <> "\n"
        <> showSep "\n" decls <> "\n"
        <> mainDecl

------------------------------------------------------------------------

genMain :: CG String
genMain = do
    runMain <- assignName (sMN 0 "runMain")
    return $ unlines [
        "Public Sub Main"
      , "    Idris_MakeOnBits"
      , "    Call " <> runMain
      , "End Sub"
      ]

nameMappings :: CGState -> String
nameMappings s = concatMap mapping
               . sort
               . map swap
               . M.toList
               $ cgNames s
  where
    mapping (i, n) = "' " <> global i <> " = " <> show n <> "\n"

header :: String
header = unlines [
     "Private Idris_OnBits(0 To 31) As Long"
   , ""
   , "Private Sub Idris_MakeOnBits()"
   , "    Dim j As Integer"
   , "    Dim v As Long"
   , "    For j = 0 To 30"
   , "        v = v + (2 ^ j)"
   , "        Idris_OnBits(j) = v"
   , "    Next j"
   , "    Idris_OnBits(j) = v + &H80000000"
   , "End Sub"
   , ""
   , "Private Function Idris_LShiftLong(ByVal value As Long, ByVal shift As Integer) As Long"
   , "    If (value And (2 ^ (31 - shift))) Then GoTo Overflow"
   , "    Idris_LShiftLong = ((value And Idris_OnBits(31 - shift)) * (2 ^ shift))"
   , "    Exit Function"
   , "Overflow:"
   , "    Idris_LShiftLong = ((value And Idris_OnBits(31 - (shift + 1))) * (2 ^ (shift))) Or &H80000000"
   , "End Function"
   , ""
   , "Private Function Idris_RShiftLong(ByVal value As Long, ByVal shift As Integer) As Long"
   , "    Dim hi As Long"
   , "    If (value And &H80000000) Then hi = &H40000000"
   , "    Idris_RShiftLong = (value And &H7FFFFFFE) \\ (2 ^ shift)"
   , "    Idris_RShiftLong = (Idris_RShiftLong Or (hi \\ (2 ^ (shift - 1))))"
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

--vbaname :: Name -> String
--vbaname n = "Idris_" <> concatMap vbachar (showCG n)
--  where
--    vbachar x | isAlpha x = [x]
--              | isDigit x = [x]
--              | x == '.'  = "_"
--              | otherwise = "_" <> show (fromEnum x) <> "_"

global :: Int -> String
global i = "G" <> show i

local :: Int -> String
local i = "L" <> show i

doCodegen :: (Name, SDecl) -> CG String
doCodegen (n, SFun _ args _ exp) = cgFun n args exp

------------------------------------------------------------------------
-- Functions

cgFun :: Name -> [Name] -> SExp -> CG String
cgFun n args exp = do
    name <- assignName n
    body <- cgExp (ret name) exp'
    return $ "Private Function " <> decl name <> "\n"
          <> tailRecLabel <> body
          <> "End Function\n"
  where
    decl name      = name <> "(" <> showSep "," (take nargs cgFunArgs) <> ")"
    ret  name rexp = name <> " = " <> rexp

    nargs = length args

    exp' = selfTailExp n exp

    tailRecLabel | hasTailExp exp' = "TailRec:\n"
                 | otherwise       = ""

cgFunArgs :: [String]
cgFunArgs = map local [0..]

------------------------------------------------------------------------
-- Tail Calls

-- | Ensure expression only has self-recursive tail calls.
selfTailExp :: Name -> SExp -> SExp
selfTailExp self exp = case exp of
    -- fixup function application
    SApp True n args | self == n -> SApp True  n args
    SApp _    n args             -> SApp False n args

    -- traverse sub-expressions
    SLet n v exp'   -> SLet n (selfTailExp self v) (selfTailExp self exp')
    SUpdate n exp'  -> SUpdate n (selfTailExp self exp')
    SCase c v alts  -> SCase c v (map (selfTailAlt self) alts)
    SChkCase v alts -> SChkCase v (map (selfTailAlt self) alts)

    -- everything else
    x -> x

-- | Ensure alternatives only have self-recursive tail calls.
selfTailAlt :: Name -> SAlt -> SAlt
selfTailAlt self alt = case alt of
    SConCase lv t n args exp -> SConCase lv t n args (selfTailExp self exp)
    SConstCase c exp         -> SConstCase c (selfTailExp self exp)
    SDefaultCase exp         -> SDefaultCase (selfTailExp self exp)

hasTailExp :: SExp -> Bool
hasTailExp exp = case exp of
    SApp isTail _ _ -> isTail
    SLet _ v exp'   -> hasTailExp v || hasTailExp exp'
    SUpdate _ exp'  -> hasTailExp exp'
    SCase _ _ alts  -> any hasTailAlt alts
    SChkCase _ alts -> any hasTailAlt alts
    _               -> False

hasTailAlt :: SAlt -> Bool
hasTailAlt alt = case alt of
    SConCase _ _ _ _ exp -> hasTailExp exp
    SConstCase _ exp     -> hasTailExp exp
    SDefaultCase exp     -> hasTailExp exp

------------------------------------------------------------------------
-- Expressions

-- cgExp converts the SExp into a chunk of vba which calculates the result
-- of an expression, then runs the function on the resulting bit of code.
--
-- We do it this way because we might calculate an expression in a deeply nested
-- case statement, or inside a let, etc, so the assignment/return of the calculated
-- expression itself may happen quite deeply.

cgExp :: (String -> String) -> SExp -> CG String
cgExp ret (SV (Loc i)) =
    return $ ret $ local i <> "\n"

cgExp ret (SV (Glob n)) = do
    name <- assignName n
    return $ ret name <> "()\n"

cgExp _ (SApp True _ args) = do
    appArgs <- mapM cgVar args
    return $ concat (zipWith assign cgFunArgs appArgs)
          <> "GoTo TailRec\n"
  where
    lv `assign` rv = lv <> " = " <> rv <> "\n"

cgExp ret (SApp False f args) = do
    name    <- assignName f
    appArgs <- mapM cgVar args
    return $ ret (name <> "(" <> showSep "," appArgs <> ")\n")

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

-- Private Declare Function socket Lib "libc.dylib" (ByVal domain As Long, ByVal type_ As Long, ByVal protocol As Long) As Long

-- Private Declare Function errno Lib "libc.dylib" Alias "__error" () As Long

nativeFFI :: String -> [(FDesc, LVar)] -> FDesc -> CG String
nativeFFI sig args rt = do
    name <- assignName (sUN sig)
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
-- Variables

cgVar :: LVar -> CG String
cgVar (Loc i)  = return (local i)
cgVar (Glob n) = assignName n

------------------------------------------------------------------------
-- Constants

cgConst :: Const -> String
cgConst x = case x of
    I   i -> show i
    BI  i -> show i
    Fl  f -> show f
    Ch  i -> show (ord i)
    B8  i -> show i
    B16 i -> show i
    B32 i -> show i
    Str s -> cgString s

    TheWorld       -> "0"
    (AType _)      -> "0"
    StrType        -> "0"
    ManagedPtrType -> "0"
    BufferType     -> "0"
    WorldType      -> "0"
    PtrType        -> "0"
    VoidType       -> "0"

    B64  _ -> error (msg "64-bit integers")
    B8V  _ -> error (msg "8-bit vectors")
    B16V _ -> error (msg "16-bit vectors")
    B32V _ -> error (msg "32-bit vectors")
    B64V _ -> error (msg "64-bit vectors")
    Forgot -> error "Tried to compile: Forgot"
  where
    msg name = name <> " not supported (tried to compile: " <> show x <> ")"

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

cgPrim :: PrimFn -> [String] -> String

cgPrim (LSExt _ _) [x] = x
cgPrim (LZExt _ _) [x] = x

cgPrim (LPlus  _) [l,r] = "(" <> l <> " + "   <> r <> ")"
cgPrim (LMinus _) [l,r] = "(" <> l <> " - "   <> r <> ")"
cgPrim (LTimes _) [l,r] = "(" <> l <> " * "   <> r <> ")"
--cgPrim (LSDiv  _) [l,r] = "(" <> l <> " \\ "  <> r <> ")"
cgPrim (LSRem  _) [l,r] = "(" <> l <> " Mod " <> r <> ")"

cgPrim (LLSHR _) [x,s] = "Idris_LShiftLong(" <> x <> ", " <> s <> ")"

cgPrim (LEq  _) [l,r] = "(" <> l <> " = "  <> r <> ")"

cgPrim (LSLt _) [l,r] = "(" <> l <> " < "  <> r <> ")"
cgPrim (LSLe _) [l,r] = "(" <> l <> " <= " <> r <> ")"
cgPrim (LSGt _) [l,r] = "(" <> l <> " > "  <> r <> ")"
cgPrim (LSGe _) [l,r] = "(" <> l <> " >= " <> r <> ")"

cgPrim (LAnd   _) [l,r] = "(" <> l <> " And "  <> r <> ")"
cgPrim (LOr    _) [l,r] = "(" <> l <> " Or  "  <> r <> ")"
cgPrim (LXOr   _) [l,r] = "(" <> l <> " Xor "  <> r <> ")"
cgPrim (LCompl _) [x]   = "(Not " <> x <> ")"

cgPrim LStrEq [l, r] = "(" <> l <> " = " <> r <> ")"

cgPrim (LIntStr _)   [x] = "CStr(" <> x <> ")"
cgPrim (LStrInt _)   [x] = "CInt(" <> x <> ")"
cgPrim (LIntFloat _) [x] = "CDbl(" <> x <> ")"
cgPrim (LFloatInt _) [x] = "CInt(" <> x <> ")"
cgPrim (LChInt _)    [x] = x
cgPrim (LIntCh _)    [x] = x
cgPrim (LTrunc _ _)  [x] = x

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
