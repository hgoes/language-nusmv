module Language.NuSMV.Syntax where

import Language.NuSMV.Misc

data Module = Module { moduleName :: String 
                     , moduleParameter :: [String]
                     , moduleBody :: [ModuleElement]
                     }
              deriving Show

data TypeSpecifier = SimpleType SimpleTypeSpecifier
                   | ModuleType String [BasicExpr]
                   | ProcessType String [BasicExpr]
                   deriving Show

data SimpleTypeSpecifier
     = TypeBool
     | TypeWord { typeWordSigned :: Maybe Bool
                , typeWordBits :: BasicExpr
                }
     | TypeEnum [Either String Integer]
     | TypeRange BasicExpr BasicExpr
     | TypeArray BasicExpr BasicExpr SimpleTypeSpecifier
     deriving Show

data Constant = ConstBool Bool
              | ConstInteger Integer
              | ConstId String
              | ConstWord WordConstant
              | ConstRange Integer Integer
              deriving Show

data BasicExpr = ConstExpr Constant
               | IdExpr ComplexIdentifier
               | BinExpr BinOp BasicExpr BasicExpr
               | UnExpr UnOp BasicExpr
               | CaseExpr [(BasicExpr,BasicExpr)]
               | SetExpr [BasicExpr]
               deriving Show

data BinOp = OpEq | OpNeq
           | OpLT | OpLTE | OpGT | OpGTE
           | OpAnd | OpOr | OpImpl | OpEquiv
           | OpUnion | OpIn
           | OpPlus | OpMinus | OpMod
           | CTLAU | CTLEU
           | LTLU | LTLV 
           deriving Show

data UnOp = OpNot
          | OpNext
          | OpToInt
          | CTLAG | CTLAF | CTLAX | CTLEX | CTLEG | CTLEF
          | LTLX | LTLF | LTLG | LTLO
          deriving Show

data ModuleElement = VarDeclaration [(String,TypeSpecifier)]
                   | DefineDeclaration [(String,BasicExpr)]
                   | AssignConstraint [(AssignType,ComplexIdentifier,BasicExpr)]
                   | FairnessConstraint FairnessType BasicExpr
                   | TransConstraint BasicExpr
                   | InitConstraint BasicExpr
                   | CTLSpec BasicExpr
                   | LTLSpec BasicExpr
                   | ComputeSpec ComputeType BasicExpr BasicExpr
                   deriving Show

data ComputeType = ComputeMIN | ComputeMAX deriving Show

data ComplexIdentifier = ComplexId { idBase :: Maybe String
                                   , idNavigation :: [Either String BasicExpr]
                                   }
                       deriving Show

data AssignType = NormalAssign
                | InitAssign
                | NextAssign
                deriving Show

data FairnessType = Justice | Fairness | Compassion deriving Show
