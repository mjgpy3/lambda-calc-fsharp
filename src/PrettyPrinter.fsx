module PrettyPrinter

#load "Ast.fsx"
open Ast

let rec pretty = function
  | Name c -> Convert.ToString c
  | Application (fn, arg) -> "(" + pretty fn + " " + pretty arg + ")"
  | Lambda (arg, body) -> "\\" + Convert.ToString arg + "." + pretty body
  | Closure (arg, body, _) -> "\\" + Convert.ToString arg + "." + pretty body
