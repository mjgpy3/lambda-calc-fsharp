module Ast

type Ast =
  | Application of Ast*Ast
  | Lambda of char*Ast
  | Closure of char*Ast*(char*Ast)list
  | Name of char
