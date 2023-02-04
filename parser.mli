
(* The type of tokens. *)

type token = 
  | VAR
  | UMINUS
  | TIMES
  | THIS
  | THEN
  | SUPER
  | STR of (string)
  | SEMICOLON
  | RPAREN
  | RETURN
  | RESULT
  | RELOP of (Ast.opComp)
  | RCBR
  | PLUS
  | OVERRIDE
  | OBJECT
  | NEW
  | MINUS
  | LPAREN
  | LCBR
  | IS
  | IF
  | IDCLASS of (string)
  | ID of (string)
  | EXTENDS
  | EOF
  | ELSE
  | DOT
  | DIV
  | DEF
  | CSTE of (int)
  | CONCATE
  | COLON
  | CLASS
  | AUTO
  | ASSIGN

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.prog)
