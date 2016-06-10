module Ast

type Ast =
  | Application of Ast*Ast
  | Lambda of char*Ast
  | Name of char
