module IRTS.CodegenVBA (codegenVBA) where

import IRTS.CodegenCommon
import IRTS.Lang
import IRTS.Simplified
import Idris.Core.TT hiding (str)
import Prelude hiding (exp)

import Data.Monoid ((<>))
import Data.Char

------------------------------------------------------------------------

codegenVBA :: CodeGenerator
codegenVBA ci = writeFile (outputFile ci) code
  where
    decls = concatMap doCodegen (simpleDecls ci)

    code = header <> "\n"
        <> decls <> "\n"
        <> footer <> "\n"

------------------------------------------------------------------------

footer :: String
footer = unlines [
     "Sub Main"
   , "    Call " <> vbaname (sMN 0 "runMain")
   , "End Sub"
   ]

header :: String
header = unlines [
--     "Option Explicit"
--   , ""
     "Function Idris_Error(msg)"
   , "    Call Err.Raise(10101, \"Idris\", msg)"
   , "End Function"
   , ""
   , "Function Idris_WriteStr(str)"
   , "    Debug.Print str"
   , "End Function"
   , ""
   , "Function Idris_ReadStr() As String"
   , "    Idris_ReadStr = InputBox(\"Input:\", \"Idris\")"
   , "End Function"
   , ""
   , "Function Idris_Append(xs, ys) As String"
   , "    Idris_Append = CStr(xs) + CStr(ys)"
   , "End Function"
   ]

------------------------------------------------------------------------

vbaname :: Name -> String
vbaname n = "Idris_" <> concatMap vbachar (showCG n)
  where
    vbachar x | isAlpha x = [x]
              | isDigit x = [x]
              | otherwise = "_" <> show (fromEnum x) <> "_"

var :: Name -> String
var n = vbaname n

loc :: Int -> String
loc i = "Loc" <> show i

doCodegen :: (Name, SDecl) -> String
doCodegen (n, SFun _ args _ def) = cgFun n args def

cgFun :: Name -> [Name] -> SExp -> String
cgFun n args def =
       "Function " <> vbaname n <> "("
                   <> showSep "," (map arg (zip [0..] args)) <> ")\n"
    <> cgBody doRet def <> "\n"
    <> "End Function\n\n"
  where
    arg (i, _) = loc i

    doRet :: String -> String -- Return the calculated expression
    doRet str = vbaname n <> " = " <> str <> "\n"
             <> "Exit Function"

-- cgBody converts the SExp into a chunk of vba which calculates the result
-- of an expression, then runs the function on the resulting bit of code.
--
-- We do it this way because we might calculate an expression in a deeply nested
-- case statement, or inside a let, etc, so the assignment/return of the calculated
-- expression itself may happen quite deeply.

cgBody :: (String -> String) -> SExp -> String
cgBody ret (SV (Glob n)) = ret $ vbaname n <> "()"
cgBody ret (SV (Loc i)) = ret $ loc i
cgBody ret (SApp _ f args) = ret $ vbaname f <> "(" <>
                                   showSep "," (map cgVar args) <> ")"
cgBody ret (SLet (Loc i) v sc) =
       cgBody (\x -> loc i <> " = " <> x <> "\n") v
    <> cgBody ret sc

cgBody ret (SUpdate _ e) =
    cgBody ret e

cgBody ret (SProj e i) =
    ret $ cgVar e <> "(" <> show (i + 1) <> ")"

cgBody ret (SCon _ t _ args) =
    ret $ "Array(" <> showSep ","
          (show t : (map cgVar args)) <> ")"

cgBody ret (SCase _ e alts) =
       "Select Case " <> scr <> "\n"
    <> showSep "\n" (map (cgAlt ret scrvar) alts) <> "\n"
    <> "End Select\n"
  where
    scrvar = cgVar e
    scr    = if any conCase alts
             then scrvar <> "(0)"
             else scrvar

    conCase (SConCase _ _ _ _ _) = True
    conCase _ = False

cgBody ret (SChkCase e alts) =
       "Select Case " <> scr <> "\n"
    <> showSep "\n" (map (cgAlt ret scrvar) alts) <> "\n"
    <> "End Select\n"
  where
    scrvar = cgVar e
    scr    = if any conCase alts
             then scrvar <> "(0)"
             else scrvar

    conCase (SConCase _ _ _ _ _) = True
    conCase _ = False

cgBody ret (SConst c) = ret $ cgConst c
cgBody ret (SOp op args) = ret $ cgOp op (map cgVar args)
cgBody ret SNothing = ret "0"
cgBody ret (SError x) = ret $ "Idris_Error(" <> show x <> ")"
cgBody ret _ = ret $ "Idris_Error(\"NOT IMPLEMENTED\")"

cgAlt :: (String -> String) -> String -> SAlt -> String
cgAlt ret _   (SConstCase t exp) =
    "Case " <> show t <> "\n"
            <> cgBody ret exp

cgAlt ret _   (SDefaultCase exp) =
    "Case Else\n" <> cgBody ret exp

cgAlt ret scr (SConCase lv t _ args exp) =
    "Case " <> show t <> "\n"
            <> project 1 lv args <> "\n"
            <> cgBody ret exp
  where
    project :: Int -> Int -> [a] -> [Char]
    project _ _ []       = ""
    project i v [_]      = project' i v
    project i v (_ : ns) = project' i v <> "\n" <> project (i + 1) (v + 1) ns

    project' i v = loc v <> " = " <> scr <> "(" <> show i <> ")"

cgVar :: LVar -> String
cgVar (Loc i)  = loc i
cgVar (Glob n) = var n

cgConst :: Const -> String
cgConst (I i) = show i
cgConst (Ch i) = show (ord i) -- Treat Char as ints, because PHP treats them as Strings...
cgConst (BI i) = show i
cgConst (Str s) = show s
cgConst TheWorld = "0"
cgConst x | isTypeConst x = "0"
cgConst x = error $ "Constant " <> show x <> " not compilable yet"

cgOp :: PrimFn -> [String] -> String
cgOp (LPlus (ATInt _)) [l, r]
     = "(" <> l <> " + " <> r <> ")"
cgOp (LMinus (ATInt _)) [l, r]
     = "(" <> l <> " - " <> r <> ")"
cgOp (LTimes (ATInt _)) [l, r]
     = "(" <> l <> " * " <> r <> ")"
cgOp (LEq (ATInt _)) [l, r]
     = "(" <> l <> " = " <> r <> ")"
cgOp (LSLt (ATInt _)) [l, r]
     = "(" <> l <> " < " <> r <> ")"
cgOp (LSLe (ATInt _)) [l, r]
     = "(" <> l <> " <= " <> r <> ")"
cgOp (LSGt (ATInt _)) [l, r]
     = "(" <> l <> " > " <> r <> ")"
cgOp (LSGe (ATInt _)) [l, r]
     = "(" <> l <> " >= " <> r <> ")"
cgOp LStrEq [l,r] = "(" <> l <> " = " <> r <> ")"

cgOp (LIntStr _) [x] = "CStr(" <> x <> ")"
cgOp (LChInt _) [x] = x
cgOp (LIntCh _) [x] = x
cgOp (LSExt _ _) [x] = x
cgOp (LTrunc _ _) [x] = x
cgOp LWriteStr [_,str] = "Idris_WriteStr(" <> str <> ")"
cgOp LReadStr [_] = "Idris_ReadStr()"
cgOp LStrConcat [l,r] = "Idris_Append(" <> l <> ", " <> r <> ")"
cgOp LStrCons [l,r] = "Idris_Append(chr(" <> l <> "), " <> r <> ")"
--cgOp op _ = "error(\"OPERATOR " <> show op <> " NOT IMPLEMENTED!!!!\")"
cgOp op _ = error ("Operator " <> show op <> " not implemented")
