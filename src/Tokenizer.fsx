module Tokenizer

#load "Token.fsx"
open Token

let alphabet =
  Set.ofSeq "abcdefghijklmnopqrstuvwxyz"

let tokenize text =
  let rec doTokenize = function
    | [] -> []
    | '\\' :: rest -> Lambda :: doTokenize rest
    | '.' :: rest -> Dot :: doTokenize rest
    | '(' :: rest -> LParen :: doTokenize rest
    | ')' :: rest -> RParen :: doTokenize rest
    | c :: rest -> if Set.contains c alphabet
                   then Name c :: doTokenize rest
                   else doTokenize rest

  [for c in text -> c] |> doTokenize
