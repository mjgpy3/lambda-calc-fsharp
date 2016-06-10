module Evaluator

#load "Ast.fsx"
open Ast

let rec lookup = function
  | (name, []) -> failwithf "Name %A not found in environment" name
  | (name, (bound, v) :: env) -> if name = bound
                                 then v
                                 else lookup (name, env)

let rec evalEnv env = function
  | Name n -> lookup (n, env)
  | Lambda (arg, body) -> Closure (arg, body, env)
  | Application (fn, arg) ->
    let closure = evalEnv env fn
    let value = evalEnv env arg

    match closure with
      | Closure (argName, body, closedEnv) ->
        evalEnv ((argName, value) :: List.append closedEnv env) body
      | _ -> failwith "Tried to evaluate non-closure"

let eval =
  evalEnv []
