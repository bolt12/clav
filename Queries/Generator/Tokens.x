{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$word = [a-zA-Z0-9_\-]

tokens :-

  $white+                       ;
  in                            { \s -> TokenIn }
  not                           { \s -> TokenNot }
  no                            { \s -> TokenNo }
  and                           { \s -> TokenAnd1 }
  &&                            { \s -> TokenAnd2 }
  \|\|                          { \s -> TokenOr1 }
  or                            { \s -> TokenOr2 }
  \<\=\>                        { \s -> TokenEquiv1 }
  iff                           { \s -> TokenEquiv2 }
  =>                            { \s -> TokenImplies1 }
  implies                       { \s -> TokenImplies2 }
  all                           { \s -> TokenAll }
  some                          { \s -> TokenSome }
  disj                          { \s -> TokenDisj }
  $word+                        { \s -> TokenWord s }
  !                             { \s -> TokenNeg }
  :                             { \s -> TokenPoints }
  \=                            { \s -> TokenEq }
  \+                            { \s -> TokenUnion }
  \-                            { \s -> TokenMinus }
  &                             { \s -> TokenIntersection }
  \,                            { \s -> TokenComma }
  \.                            { \s -> TokenComp }
  \|                            { \s -> TokenBar }
  \{                            { \s -> TokenOB }
  \}                            { \s -> TokenCB }
  \(                            { \s -> TokenOP }
  \)                            { \s -> TokenCP }

{

-- The token type:

data Token = TokenIn
           | TokenNot
           | TokenNo
           | TokenAnd1
           | TokenAnd2
           | TokenOr1
           | TokenOr2
           | TokenEquiv1
           | TokenEquiv2
           | TokenImplies1
           | TokenImplies2
           | TokenAll
           | TokenSome
           | TokenWord String
           | TokenNeg
           | TokenPoints
           | TokenNEq
           | TokenEq
           | TokenUnion
           | TokenMinus
           | TokenIntersection
           | TokenComma
           | TokenComp
           | TokenBar
           | TokenOB
           | TokenCB
           | TokenOP
           | TokenCP
           | TokenDisj
           deriving (Eq,Show)

scanTokens = alexScanTokens

}
