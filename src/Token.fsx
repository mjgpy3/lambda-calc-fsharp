module Token

type Token =
  | LParen
  | RParen
  | Lambda
  | Dot
  | Name of char
