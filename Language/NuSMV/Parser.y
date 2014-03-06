{
module Language.NuSMV.Parser where

import Language.NuSMV.Tokens as T
import Language.NuSMV.Syntax
}

%name nusmv
%tokentype { Token }
%error { parseError }

%token
  AF             { Key KeyAF }
  AG             { Key KeyAG }
  array          { Key Keyarray }
  ASSIGN         { Key KeyASSIGN }
  AX             { Key KeyAX }
  A              { Key KeyA }
  boolean        { Key Keyboolean }
  case           { Key Keycase }
  COMPASSION     { Key KeyCOMPASSION }
  COMPUTE        { Key KeyCOMPUTE }
  DEFINE         { Key KeyDEFINE }
  EF             { Key KeyEF }
  EG             { Key KeyEG }
  esac           { Key Keyesac }
  EX             { Key KeyEX }
  E              { Key KeyE }
  F              { Key KeyF }
  FAIRNESS       { Key KeyFAIRNESS }
  FALSE          { Key KeyFALSE }                 
  G              { Key KeyG }
  in             { Key Keyin }
  init           { Key Keyinit }
  INIT           { Key KeyINIT }
  JUSTICE        { Key KeyJUSTICE }
  LTLSPEC        { Key KeyLTLSPEC }
  MAX            { Key KeyMAX }
  MIN            { Key KeyMIN }
  mod            { Key Keymod }
  MODULE         { Key KeyMODULE }
  next           { Key Keynext }
  of             { Key Keyof }
  O              { Key KeyO }
  process        { Key Keyprocess }
  self           { Key Keyself }
  SPEC           { Key KeySPEC }
  toint          { Key Keytoint }
  TRANS          { Key KeyTRANS }
  TRUE           { Key KeyTRUE }
  union          { Key Keyunion }
  U              { Key KeyU }
  VAR            { Key KeyVAR }
  X              { Key KeyX }
  identifier     { Identifier $$ }
  integer_number { Number $$ }
  "("            { Sym (Bracket Parentheses False) }
  ")"            { Sym (Bracket Parentheses True) }
  "["            { Sym (Bracket Square False) }
  "]"            { Sym (Bracket Square True) }
  "{"            { Sym (Bracket Curly False) }
  "}"            { Sym (Bracket Curly True) }
  ","            { Sym Comma }
  ".."           { Sym DotDot }
  "."            { Sym Dot }
  ":"            { Sym Colon }
  ";"            { Sym Semicolon }
  ":="           { Sym Assign }
  "="            { Sym LEq }
  "&"            { Sym LAnd }
  "|"            { Sym LOr }
  "!"            { Sym LNot }
  "+"            { Sym Plus }
  "-"            { Sym Minus }
  "<"            { Sym T.LT }
  "<="           { Sym T.LTE }
  ">"            { Sym T.GT }
  ">="           { Sym T.GTE }
  "->"           { Sym LImpl }
  "!="           { Sym LNEq }
  "<->"          { Sym LEquiv }
                 
%left "[" "]" ","
%left X F O G
%left AG AF AX EX EF
%left "!"
%left mod
%left "+" "-"
%left union
%left in
%left "=" "!=" "<" "<=" ">" ">="
%left "&"
%left "|"
%left "<->"
%right "->"

%%

module_list : module module_list { $1:$2 }
            |                    { [] }

module : MODULE identifier opt_module_parameters module_body { Module $2 $3 $4 }

opt_module_parameters : "(" ")"                    { [] }
                      | "(" module_parameters ")"  { $2 }
                      |                            { [] }
                                                   
module_parameters : identifier "," module_parameters { $1:$3 }
                  | identifier                       { [$1] }

module_body : module_element module_body { $1:$2 }
            |                            { [] }

module_element : var_declaration       { $1 }
               | define_declaration    { $1 }
               | assign_constraint     { $1 }
               | fairness_constraint   { $1 }
               | ctl_specification     { $1 }
               | ltl_specification     { $1 }
               | trans_constraint      { $1 }
               | init_constraint       { $1 }
               | compute_specification { $1 }

var_declaration : VAR var_list { VarDeclaration $2 }

assign_constraint : ASSIGN assign_list { AssignConstraint $2 }

opt_semi : ";" {}
         |     {}

fairness_constraint : FAIRNESS basic_expr opt_semi   { FairnessConstraint Fairness $2 }
                    | JUSTICE basic_expr opt_semi    { FairnessConstraint Justice $2 }
                    | COMPASSION basic_expr opt_semi { FairnessConstraint Compassion $2 }

ctl_specification : SPEC basic_expr opt_semi { CTLSpec $2 }

ltl_specification : LTLSPEC basic_expr opt_semi { LTLSpec $2 }

trans_constraint : TRANS basic_expr opt_semi { TransConstraint $2 }

init_constraint : INIT basic_expr opt_semi { InitConstraint $2 }

define_declaration : DEFINE define_body { DefineDeclaration $2 }

compute_specification : COMPUTE compute_expr opt_semi { $2 }

compute_expr : MIN "[" basic_expr "," basic_expr "]" { ComputeSpec ComputeMIN $3 $5 }
             | MAX "[" basic_expr "," basic_expr "]" { ComputeSpec ComputeMAX $3 $5 }

var_list : identifier ":" type_specifier ";" var_list { ($1,$3):$5 }
         |                                            { [] }

assign_list : assign ";" assign_list { $1:$3 }
            |                        { [] }

define_body : identifier ":=" basic_expr ";" define_body { ($1,$3):$5 }
            |                                            { [] }

assign : complex_identifier ":=" basic_expr              { (NormalAssign,$1,$3) }
       | init "(" complex_identifier ")" ":=" basic_expr { (InitAssign,$3,$6) }
       | next "(" complex_identifier ")" ":=" basic_expr { (NextAssign,$3,$6) }

type_specifier : simple_type_specifier                 { SimpleType $1 }
               | identifier opt_parameter_list         { ModuleType $1 $2 }
               | process identifier opt_parameter_list { ProcessType $2 $3 }

opt_parameter_list : "(" ")"                { [] }
                   | "(" parameter_list ")" { $2 }
                   |                        { [] }

parameter_list : basic_expr "," parameter_list { $1:$3 }
               | basic_expr                    { [$1] }

simple_type_specifier : boolean                       { TypeBool }
                      | "{" enumeration_type_body "}" { TypeEnum $2 }
                      -- | basic_expr ".." basic_expr    { TypeRange $1 $3 }
                      | integer_number ".." integer_number  { TypeRange (ConstExpr $ ConstInteger $1) (ConstExpr $ ConstInteger $3) }
                      | array integer_number ".." integer_number of simple_type_specifier { TypeArray (ConstExpr $ ConstInteger $2) (ConstExpr $ ConstInteger $4) $6 }

enumeration_type_body : enumeration_type_value "," enumeration_type_body { $1:$3 }
                      | enumeration_type_value                           { [$1] }

enumeration_type_value : identifier     { Left $1 }
                       | integer_number { Right $1 }

constant : FALSE                              { ConstBool False }
         | TRUE                               { ConstBool True }
         | integer_number                     { ConstInteger $1 }
         | integer_number ".." integer_number { ConstRange $1 $3 }

basic_expr : constant                          { ConstExpr $1 }
           | complex_identifier                { IdExpr $1 }
           | "(" basic_expr ")"                { $2 }
           | basic_expr "=" basic_expr         { BinExpr OpEq $1 $3 }
           | basic_expr "!=" basic_expr        { BinExpr OpNeq $1 $3 }             
           | basic_expr "&" basic_expr         { BinExpr OpAnd $1 $3 }
           | basic_expr "|" basic_expr         { BinExpr OpOr $1 $3 }
           | basic_expr "->" basic_expr        { BinExpr OpImpl $1 $3 }
           | basic_expr "<->" basic_expr       { BinExpr OpEquiv $1 $3 }
           | basic_expr "+" basic_expr         { BinExpr OpPlus $1 $3 }
           | basic_expr "-" basic_expr         { BinExpr OpMinus $1 $3 }
           | basic_expr mod basic_expr         { BinExpr OpMod $1 $3 }
           | basic_expr "<" basic_expr         { BinExpr OpLT $1 $3 }
           | basic_expr "<=" basic_expr        { BinExpr OpLTE $1 $3 }
           | basic_expr ">" basic_expr         { BinExpr OpGT $1 $3 }
           | basic_expr ">=" basic_expr        { BinExpr OpGTE $1 $3 }
           | "!" basic_expr                    { UnExpr OpNot $2 }
           | basic_expr union basic_expr       { BinExpr OpUnion $1 $3 }
           | basic_expr in basic_expr          { BinExpr OpIn $1 $3 }
           | case case_body esac               { CaseExpr $2 }
           | next "(" basic_expr ")"           { UnExpr OpNext $3 }
           | toint "(" basic_expr ")"          { UnExpr OpToInt $3 }
           | "{" basic_expr_list "}"           { SetExpr $2 }
           | AF basic_expr                     { UnExpr CTLAF $2 }
           | AG basic_expr                     { UnExpr CTLAG $2 }
           | AX basic_expr                     { UnExpr CTLAX $2 }
           | EF basic_expr                     { UnExpr CTLEF $2 }
           | EX basic_expr                     { UnExpr CTLEX $2 }
           | A "[" basic_expr U basic_expr "]" { BinExpr CTLAU $3 $5 }
           | E "[" basic_expr U basic_expr "]" { BinExpr CTLEU $3 $5 }
           | F basic_expr                      { UnExpr LTLF $2 }
           | G basic_expr                      { UnExpr LTLG $2 }
           | X basic_expr                      { UnExpr LTLX $2 }
           | O basic_expr                      { UnExpr LTLO $2 }

basic_expr_list : basic_expr "," basic_expr_list { $1:$3 }
                | basic_expr                     { [$1] }

case_body : basic_expr ":" basic_expr ";" case_body { ($1,$3):$5 }
          |                                         { [] }                  

complex_identifier : complex_identifier_base complex_identifier_navigation { ComplexId $1 $2 }

complex_identifier_base : identifier { Just $1 }
                        | self       { Nothing }

complex_identifier_navigation : "." identifier complex_identifier_navigation     { (Left $2):$3 }
                              | "[" basic_expr "]" complex_identifier_navigation { (Right $2):$4 }
                              |                                                  { [] }

{
parseError xs = error ("Parse error at "++show (take 5 xs))
}
