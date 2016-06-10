module Parser

#load "Token.fsx"
#load "Ast.fsx"

module A = Ast
module T = Token

let rec parseSingle = function
  | T.Name name :: rest -> A.Name name, rest
  | T.Lambda :: T.Name name :: Dot :: rest ->
    let body, rest2 = parseSingle rest

    A.Lambda (name, body), rest2
  | T.LParen :: rest ->
    let fn, rest1 = parseSingle rest
    let arg, rest2 = parseSingle rest1

    A.Application (fn, arg), rest2

let parse = fst << parseSingle
