{
module Language.NuSMV.Lexer where

import Language.NuSMV.Misc
import Language.NuSMV.Tokens as T
}

%wrapper "basic"

tokens :- 
  $white+                                                           ;
  "--" .*                                                           ;
  AF                                                                { key KeyAF }
  AG                                                                { key KeyAG }
  array                                                             { key Keyarray }
  ASSIGN                                                            { key KeyASSIGN }
  AX                                                                { key KeyAX }
  A                                                                 { key KeyA }
  boolean                                                           { key Keyboolean }
  case                                                              { key Keycase }
  COMPASSION                                                        { key KeyCOMPASSION }
  COMPUTE                                                           { key KeyCOMPUTE }
  DEFINE                                                            { key KeyDEFINE }
  EF                                                                { key KeyEF }
  esac                                                              { key Keyesac }
  EX                                                                { key KeyEX }
  E                                                                 { key KeyE }
  F                                                                 { key KeyF }
  FAIRNESS                                                          { key KeyFAIRNESS }
  FALSE                                                             { key KeyFALSE }
  G                                                                 { key KeyG }
  in                                                                { key Keyin }
  init                                                              { key Keyinit }
  INIT                                                              { key KeyINIT }
  LTLSPEC                                                           { key KeyLTLSPEC }
  MAX                                                               { key KeyMAX }
  MIN                                                               { key KeyMIN }
  mod                                                               { key Keymod }
  MODULE                                                            { key KeyMODULE }
  next                                                              { key Keynext }
  of                                                                { key Keyof }
  O                                                                 { key KeyO }
  JUSTICE                                                           { key KeyJUSTICE }
  process                                                           { key Keyprocess }
  self                                                              { key Keyself }
  SPEC                                                              { key KeySPEC }
  toint                                                             { key Keytoint }
  TRANS                                                             { key KeyTRANS }
  TRUE                                                              { key KeyTRUE }
  union                                                             { key Keyunion }
  U                                                                 { key KeyU }
  VAR                                                               { key KeyVAR }
  X                                                                 { key KeyX }
  [a-zA-Z\_] [a-zA-Z0-9\_\$\#\-]*                                   { Identifier }
  \-? [0-9]+                                                        { \s -> Number (read s) }
  0 (s | u)? (b | B | o | O | d | D | h | H) [0-9]* \_ [a-fA-F0-9]+ { parseWordConstant }
  "("                                                               { sym $ Bracket Parentheses False }
  ")"                                                               { sym $ Bracket Parentheses True }
  "{"                                                               { sym $ Bracket Curly False }
  "}"                                                               { sym $ Bracket Curly True }
  "["                                                               { sym $ Bracket Square False }
  "]"                                                               { sym $ Bracket Square True }
  ","                                                               { sym Comma }
  ".."                                                              { sym DotDot }
  ":"                                                               { sym Colon }
  ";"                                                               { sym Semicolon }
  ":="                                                              { sym Assign }
  "!="                                                              { sym LNEq }
  "="                                                               { sym LEq }
  "&"                                                               { sym LAnd }
  "|"                                                               { sym LOr }
  "!"                                                               { sym LNot }
  "."                                                               { sym Dot }
  "<->"                                                             { sym LEquiv }
  "<="                                                              { sym T.LTE }
  "<"                                                               { sym T.LT }
  ">="                                                              { sym T.GTE }
  ">"                                                               { sym T.GT }
  "+"                                                               { sym Plus }
  "-"                                                               { sym Minus }
  "->"                                                              { sym LImpl }

{
key :: Keyword -> String -> Token
key k _ = Key k

parseWordConstant :: String -> Token
parseWordConstant ('0':r1) = WordConst $ WordConstant sgn bts (read r3)
  where
    (sgn,r2) = case r1 of
      's':r -> (Just True,r)
      'u':r -> (Just False,r)
      _ -> (Nothing,r1)
    (bts,r3) = case break (=='_') r2 of
      ("",_:r) -> (Nothing,r)
      (nr,_:r) -> (Just $ read nr,r)

sym :: Symbol -> String -> Token
sym s _ = Sym s

}