
(* The type of tokens. *)

type token = 
  | VARIABLE of (string)
  | TIMES
  | RPAREN
  | RBRACKET
  | PLUS
  | NAT
  | NAME of (string)
  | MINUS
  | LPAREN
  | LESS
  | LEQ
  | LBRACKET
  | INTEGER of (int)
  | INPUT_OUTPUT
  | GREATER
  | GEQ
  | EQUATION
  | EQUAL
  | EOF
  | ENTRY
  | DOT
  | COMMA
  | COLON

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val crs: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Input_types.clause list)
