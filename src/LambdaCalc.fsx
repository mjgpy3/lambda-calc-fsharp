module LambdaCalc

type Token =
  | LParen
  | RParen
  | LambdaT
  | Dot
  | NameT of char

type Ast =
  | Application of Ast*Ast
  | Lambda of char*Ast
  | Closure of char*Ast*(char*Ast)list
  | Name of char

let rec pretty = function
  | Name c -> Convert.ToString c
  | Application (fn, arg) -> "(" + pretty fn + " " + pretty arg + ")"
  | Lambda (arg, body) -> "\\" + Convert.ToString arg + "." + pretty body
  | Closure (arg, body, _) -> "\\" + Convert.ToString arg + "." + pretty body

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

let rec parseSingle = function
  | NameT name :: rest -> Name name, rest
  | LambdaT :: NameT name :: Dot :: rest ->
    let body, rest2 = parseSingle rest

    Lambda (name, body), rest2
  | Token.LParen :: rest ->
    let fn, rest1 = parseSingle rest
    let arg, rest2 = parseSingle rest1

    Application (fn, arg), rest2

let parse = fst << parseSingle

let alphabet =
  Set.ofSeq "abcdefghijklmnopqrstuvwxyz"

let tokenize text =
  let rec doTokenize = function
    | [] -> []
    | '\\' :: rest -> LambdaT :: doTokenize rest
    | '.' :: rest -> Dot :: doTokenize rest
    | '(' :: rest -> LParen :: doTokenize rest
    | ')' :: rest -> RParen :: doTokenize rest
    | c :: rest -> if Set.contains c alphabet
                   then NameT c :: doTokenize rest
                   else doTokenize rest

  [for c in text -> c] |> doTokenize

let interp =
  tokenize >>
  parse >>
  eval >>
  pretty

[<EntryPoint>]
let main args =
  let result = interp args.[0]
  printfn "%A" result
  0
