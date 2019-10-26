{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-z]
$asterisc = '

tokens :-

  $white+                    ;
  \(                         { \_ -> LeftP }
  \)                         { \_ -> RightP }
  \\                         { \_ -> LambdaT }
  \.                         { \_ -> DotT }
  $alpha [$alpha $digit $asterisc]*    { \s -> Ident s }

{

data Token = DotT
           | LambdaT
           | LeftP
           | RightP
           | Ident String
           deriving (Show, Eq)

}

