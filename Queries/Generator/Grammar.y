{
module Grammar where

import Tokens
}

%name parseAlloy
%tokentype { Token }
%error { parseError }

%token
    word     { TokenWord $$ }
    in       { TokenIn }
    not      { TokenNot }
    no       { TokenNo }
    and1     { TokenAnd1 }
    and2     { TokenAnd2 }
    or1      { TokenOr1 }
    or2      { TokenOr2 }
    equiv1   { TokenEquiv1 }
    equiv2   { TokenEquiv2 }
    implies1 { TokenImplies1 }
    implies2 { TokenImplies2 }
    all      { TokenAll }
    some     { TokenSome }
    '!'      { TokenNeg }
    ':'      { TokenPoints }
    '='      { TokenEq }
    '+'      { TokenUnion }
    '-'      { TokenMinus }
    '{'      { TokenOB }
    '}'      { TokenCB }
    '('      { TokenOP }
    ')'      { TokenCP }
    '&'      { TokenIntersection }
    ','      { TokenComma }
    '.'      { TokenComp }
    '|'      { TokenBar }
    disj     { TokenDisj }

%left all no some
%left or1 or2
%left equiv1 equiv2
%right implies1 implies2
%left and1 and2
%left '!'
%left in '=' not
%left UNOP
%left no some
%left '+' '-'
%left '&'
%left '.'

%%

Exp : Quant Decl BlockOrBar { EQuant $1 $2 $3 }
    | UnOp Exp              { EUnOp $1 $2 }
    | Exp BinOp Exp         { EBinOp $2 $1 $3 }
    | Exp CompareOp Exp     { EComp $2 $1 $3 }
    | Exp '!' CompareOp Exp { ENComp $3 $1 $4 }
    | Exp not CompareOp Exp { ENComp $3 $1 $4 }
    | '(' Exp ')'           { EPar $2 }
    | word                  { EWord $1 }
    | Block                 { EBlock $1 }

Decl : Words ':' Exp           { DeclW $1 $3 }
     | Words ':' disj Exp      { DeclWD $1 $4 }
     | disj Words ':' Exp      { DeclDW $2 $4 } 
     | disj Words ':' disj Exp { DeclDWD $2 $5 }
     | Decl ',' Words ':' Exp           { DeclWR $3 $5 $1 }
     | Decl ',' Words ':' disj Exp      { DeclWDR $3 $6 $1 }
     | Decl ',' disj Words ':' Exp      { DeclDWR $4 $6 $1 } 
     | Decl ',' disj Words ':' disj Exp { DeclDWDR $4 $7 $1 }

Quant : all  { All }
      | no   { QNo }
      | some { QSome }

BinOp : or1      { Or }
      | or2      { Or }
      | and1     { And }
      | and2     { And }
      | '&'      { Intersection }
      | equiv1   { Iff }
      | equiv2   { Iff }
      | implies1 { Implies }
      | implies2 { Implies }
      | '+'      { Union }
      | '-'      { Minus }
      | '.'      { Comp }

CompareOp : '=' { Eq }
          | in  { In } 

UnOp : '!'             { Neg } 
     | not             { Not }
     | no   %prec UNOP { ONo }
     | some %prec UNOP { OSome }
     -- '~' Converse not supported


Block : '{' '}'     { EmptyBlock }
      | '{' Exp '}' { Block $2 }

Words : Words ',' word { $1 ++ [$3]}
      | word           { [$1] }

BlockOrBar : Block   { BlockB $1 }
           | '|' Exp { BBar $2 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data Exp = EQuant Quant Decl BlockOrBar
         | EUnOp UnOp Exp
         | EBinOp BinOp Exp Exp
         | EComp CompareOp Exp Exp
         | ENComp CompareOp Exp Exp
         | EBlock Block
         | EPar Exp
         | EWord String
         deriving Show

data Quant = All 
           | QNo 
           | QSome
           deriving Show

data Decl = DeclW Words Exp
          | DeclWD Words Exp
          | DeclDW Words Exp
          | DeclDWD Words Exp
          | DeclWR Words Exp Decl
          | DeclWDR Words Exp Decl
          | DeclDWR Words Exp Decl 
          | DeclDWDR Words Exp Decl
          deriving Show

data BlockOrBar = BlockB Block
                | BBar Exp
                deriving Show

type Words = [String]

data UnOp = Neg 
          | Id
          | Not 
          | ONo 
          | OSome
          deriving Show

data BinOp = Or 
           | And 
           | Intersection 
           | Iff 
           | Implies 
           | Union 
           | Minus 
           | Comp
           deriving Show

data CompareOp = Eq 
               | In
               deriving Show

data Block = EmptyBlock
           | Block Exp
           deriving Show

}
