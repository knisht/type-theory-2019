{
module Parser where

import Grammar
import Lexer
}

%name      parseLambda
%tokentype { Token }
%error     { parseError }
%monad     { Either String }{ >>= }{ return }

%token IDENT  { Ident $$ }
%token LAMBDA { LambdaT }
%token DOT    { DotT }
%token LEFTP  { LeftP }
%token RIGHTP { RightP }

%%

Expr
  : Appl                       { $1 }
  | Lambda                     { $1 }
  | Appl Lambda                { Appl $1 $2 }

Lambda
  : LAMBDA IDENT DOT Expr      { Lambda $2 $4 }

Appl
  : Atom                       { $1 }
  | Appl Atom                  { Appl $1 $2 }

Atom
  : Var                        { $1 }
  | LEFTP Expr RIGHTP          { $2 }

Var
  : IDENT                      { Var $1 }

{
parseError = fail "Parse error"
}
