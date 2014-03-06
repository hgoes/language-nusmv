module Language.NuSMV.Pretty where

import Language.NuSMV.Syntax
import Language.NuSMV.Misc

import Text.PrettyPrint

prettyProgram :: [Module] -> Doc
prettyProgram ms = vcat $ fmap prettyModule ms

prettyModule :: Module -> Doc
prettyModule m 
  = text "MODULE"
    <+> text (moduleName m)
    <> (case moduleParameter m of
           [] -> empty
           xs -> parens $ hsep $ punctuate comma $ fmap text xs)
    $+$ nest 2 (vcat (fmap prettyModuleElement (moduleBody m)))
    
prettyModuleElement :: ModuleElement -> Doc
prettyModuleElement (VarDeclaration decls)
  = text "VAR" 
    $+$ nest 2 (vcat $ fmap (\(name,tp) -> text name <+> colon <+> prettyType tp <> semi) decls)
prettyModuleElement (AssignConstraint assgn)
  = text "ASSIGN" $+$
    nest 2 (vcat $ fmap 
            (\(tp,ci,e) -> (case tp of
                               NormalAssign -> prettyId ci
                               InitAssign -> text "init" <> parens (prettyId ci)
                               NextAssign -> text "next" <> parens (prettyId ci)
                           ) <+> text ":=" <+> prettyBasicExpr 0 e <> semi)
            assgn)
prettyModuleElement (DefineDeclaration def)
  = text "DEFINE" $+$
    nest 2 (vcat $ fmap (\(name,expr) -> text name <+> text ":=" <+> prettyBasicExpr 0 expr) def)
prettyModuleElement (FairnessConstraint ft expr)
  = (case ft of
        Justice -> text "JUSTICE"
        Fairness -> text "FAIRNESS"
        Compassion -> text "COMPASSION") <+> prettyBasicExpr 0 expr
prettyModuleElement (TransConstraint expr)
  = text "TRANS" $+$ nest 2 (prettyBasicExpr 0 expr)
prettyModuleElement (InitConstraint expr)
  = text "INIT" $+$ nest 2 (prettyBasicExpr 0 expr)
prettyModuleElement (CTLSpec expr)
  = text "CTLSPEC" $+$ nest 2 (prettyBasicExpr 0 expr)
prettyModuleElement (LTLSpec expr)
  = text "LTLSPEC" $+$ nest 2 (prettyBasicExpr 0 expr)
prettyModuleElement (ComputeSpec tp e1 e2)
  = text "COMPUTE" <+> (case tp of
                           ComputeMIN -> text "MIN"
                           ComputeMAX -> text "MAX") <+> (brackets $ (prettyBasicExpr 0 e1) <> comma <+> (prettyBasicExpr 0 e2))

prettyType :: TypeSpecifier -> Doc
prettyType (SimpleType tp) = prettySimpleType tp
prettyType (ModuleType name args) = text name <> (case args of
                                                     [] -> empty
                                                     _ -> parens $ hsep $ punctuate comma $ fmap (prettyBasicExpr 0) args)
prettyType (ProcessType name args) = text "process" <+> text name <> (case args of
                                                                         [] -> empty
                                                                         _ -> parens $ hsep $ punctuate comma $ fmap (prettyBasicExpr 0) args)

prettySimpleType :: SimpleTypeSpecifier -> Doc
prettySimpleType TypeBool = text "boolean"
prettySimpleType (TypeWord sgn bts)
  = (case sgn of
        Nothing -> empty
        Just True -> text "signed"
        Just False -> text "unsigned") <+>
    text "word" <> brackets (prettyBasicExpr 0 bts)
prettySimpleType (TypeEnum vals)
  = braces (hsep $ punctuate comma $ fmap (either text integer) vals)
prettySimpleType (TypeRange from to) = (prettyBasicExpr 0 from) <>  text ".." <> (prettyBasicExpr 0 to)
prettySimpleType (TypeArray from to tp) = text "array" <+> (prettyBasicExpr 0 from) <> text ".." <> (prettyBasicExpr 0 to) <+> text "of" <+> (prettySimpleType tp)

prettyId :: ComplexIdentifier -> Doc
prettyId i = (case idBase i of
                 Nothing -> text "self"
                 Just n -> text n) <>
             (hcat $ fmap (either 
                           (\n -> char '.' <> text n)
                           (brackets . prettyBasicExpr 0)
                          ) (idNavigation i))

prettyBasicExpr :: Integer -> BasicExpr -> Doc
prettyBasicExpr _ (ConstExpr c) = prettyConstant c
prettyBasicExpr _ (IdExpr i) = prettyId i
prettyBasicExpr p (BinExpr op lhs rhs) 
  = (if p >= binOpPrecedence op
     then parens
     else id) $ prettyBinOp op 
    (prettyBasicExpr (binOpPrecedence op) lhs)
    (prettyBasicExpr (binOpPrecedence op) rhs)
prettyBasicExpr p (UnExpr op expr) 
  = (if p >= unOpPrecedence op
     then parens
     else id) $ prettyUnOp op (prettyBasicExpr (unOpPrecedence op) expr)
prettyBasicExpr p (SetExpr lst)
  = braces $ hsep $ punctuate comma $ fmap (prettyBasicExpr 0) lst
prettyBasicExpr p (CaseExpr cases)
  = text "case" $+$
    nest 2 (vcat $ fmap (\(cond,res) -> (prettyBasicExpr 0 cond) <+> 
                                        colon <+>
                                        (prettyBasicExpr 0 res) <> semi) cases)
    $+$ text "esac"

prettyBinOp :: BinOp -> Doc -> Doc -> Doc
prettyBinOp OpEq l r = l <+> char '=' <+> r
prettyBinOp OpNeq l r = l <+> text "!=" <+> r
prettyBinOp OpLT l r = l <+> char '<' <+> r
prettyBinOp OpLTE l r = l <+> text "<=" <+> r
prettyBinOp OpGT l r = l <+> char '>' <+> r
prettyBinOp OpGTE l r = l <+> text ">=" <+> r
prettyBinOp OpAnd l r = l <+> char '&' <+> r
prettyBinOp OpOr l r = l <+> char '|' <+> r
prettyBinOp OpImpl l r = l <+> text "->" <+> r
prettyBinOp OpEquiv l r = l <+> text "<->" <+> r
prettyBinOp OpUnion l r = l <+> text "union" <+> r
prettyBinOp OpIn l r = l <+> text "in" <+> r
prettyBinOp OpPlus l r = l <+> char '+' <+> r
prettyBinOp OpMinus l r = l <+> char '-' <+> r
prettyBinOp OpMod l r = l <+> text "mod" <+> r
prettyBinOp CTLAU l r = char 'A' <> brackets (l <+> char 'U' <+> r)
prettyBinOp CTLEU l r = char 'E' <> brackets (l <+> char 'U' <+> r)
prettyBinOp LTLU l r = l <+> char 'U' <+> r
prettyBinOp LTLV l r = l <+> char 'V' <+> r

prettyUnOp :: UnOp -> Doc -> Doc
prettyUnOp OpNot e = char '!' <> e
prettyUnOp OpNext e = text "next" <> parens e
prettyUnOp OpToInt e = text "toint" <> parens e
prettyUnOp CTLAG e = text "AG" <+> e
prettyUnOp CTLAF e = text "AF" <+> e
prettyUnOp CTLAX e = text "AX" <+> e
prettyUnOp CTLEX e = text "EX" <+> e
prettyUnOp CTLEF e = text "EF" <+> e
prettyUnOp LTLX e = text "X" <+> e
prettyUnOp LTLF e = text "F" <+> e
prettyUnOp LTLO e = text "O" <+> e
prettyUnOp LTLG e = text "G" <+> e

binOpPrecedence :: BinOp -> Integer
binOpPrecedence op = case op of
  OpEq -> 6
  OpNeq -> 6
  OpLT -> 6
  OpLTE -> 6
  OpGT -> 6
  OpGTE -> 6
  OpAnd -> 5
  OpOr -> 4
  OpImpl -> 1
  OpEquiv -> 2
  OpUnion -> 8
  OpIn -> 7
  OpPlus -> 10
  OpMinus -> 10
  OpMod -> 11
  _ -> 0

unOpPrecedence :: UnOp -> Integer
unOpPrecedence op = case op of
  OpNot -> 14
  _ -> 0

prettyConstant :: Constant -> Doc
prettyConstant (ConstBool True) = text "TRUE"
prettyConstant (ConstBool False) = text "FALSE"
prettyConstant (ConstInteger i) = integer i
prettyConstant (ConstId p) = text p
prettyConstant (ConstWord wc) 
  = char '0'
    <> (case wcSigned wc of
           Nothing -> empty
           Just False -> char 'u'
           Just True -> char 's')
    <> char 'd'
    <> (case wcBits wc of
           Nothing -> empty
           Just b -> integer b)
    <> char '_'
    <> integer (wcValue wc)
prettyConstant (ConstRange f t) = integer f <> text ".." <> integer t
