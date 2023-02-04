# 1 "lexer.mll"
 
open Ast
open Parser
open Lexing
exception Eof

let next_line lexbuf = Lexing.new_line lexbuf

let keyword_table = Hashtbl.create 16
let _ =
 List.iter
    (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [ "if", IF;
      "then", THEN;
      "else", ELSE;
      "class", CLASS;
      "is", IS;
      "var", VAR;
      "auto", AUTO;
      "def", DEF;
      
      "extends",EXTENDS;
      "new",NEW;
      "object",OBJECT;
      "override",OVERRIDE;
      "return" , RETURN;
      "super", SUPER;
      "this", THIS;
      "result", RESULT
    ]

# 34 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\252\255\253\255\254\255\000\000\255\255\001\000\246\255\
    \002\000\000\000\255\255\249\255\250\255\251\255\252\255\253\255\
    \254\255\077\000\228\255\229\255\231\255\000\000\001\000\003\000\
    \238\255\239\255\240\255\241\255\242\255\243\255\245\255\246\255\
    \247\255\248\255\249\255\001\000\000\000\252\255\162\000\237\000\
    \000\000\250\255\236\255\230\255\234\255\232\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\003\000\255\255\255\255\255\255\
    \007\000\008\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\022\000\020\000\018\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\011\000\004\000\255\255\002\000\001\000\
    \000\000\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\001\000\000\000\000\000\000\000\255\255\000\000\008\000\000\000\
    \008\000\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\018\000\000\000\000\000\000\000\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\000\000\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\040\000\003\000\000\000\000\000\040\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \040\000\000\000\016\000\010\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\004\000\041\000\000\000\000\000\000\000\005\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\000\000\000\000\000\000\045\000\044\000\043\000\
    \042\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\040\000\037\000\
    \000\000\000\000\040\000\000\000\015\000\009\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\014\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\040\000\013\000\034\000\
    \000\000\000\000\012\000\031\000\011\000\029\000\028\000\030\000\
    \033\000\000\000\032\000\026\000\035\000\036\000\036\000\036\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\023\000\
    \024\000\022\000\020\000\021\000\000\000\000\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\038\000\038\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \027\000\000\000\025\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\000\000\000\000\000\000\
    \002\000\007\000\255\255\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \000\000\000\000\000\000\000\000\000\000\019\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\040\000\000\000\255\255\255\255\040\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \040\000\255\255\009\000\006\000\008\000\255\255\255\255\255\255\
    \255\255\255\255\000\000\035\000\255\255\255\255\255\255\004\000\
    \036\000\036\000\036\000\036\000\036\000\036\000\036\000\036\000\
    \036\000\036\000\255\255\255\255\255\255\021\000\022\000\022\000\
    \023\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\017\000\017\000\
    \255\255\255\255\017\000\255\255\009\000\006\000\008\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\009\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\017\000\009\000\017\000\
    \255\255\255\255\009\000\017\000\009\000\017\000\017\000\017\000\
    \017\000\255\255\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\255\255\255\255\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \017\000\017\000\017\000\017\000\017\000\017\000\017\000\017\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\017\000\017\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \017\000\255\255\017\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\255\255\255\255\255\255\
    \000\000\006\000\008\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\038\000\038\000\038\000\
    \038\000\038\000\038\000\038\000\038\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \255\255\255\255\255\255\255\255\255\255\017\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \039\000\039\000\039\000\039\000\039\000\039\000\039\000\039\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec comment lexbuf =
   __ocaml_lex_comment_rec lexbuf 0
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 45 "lexer.mll"
                  ( 
                    token lexbuf
                  )
# 206 "lexer.ml"

  | 1 ->
# 48 "lexer.mll"
                   ( 
                     new_line lexbuf; comment lexbuf
                   )
# 213 "lexer.ml"

  | 2 ->
# 51 "lexer.mll"
                   ( 
                     raise (MISC_Error "unclosed comment")
                   )
# 220 "lexer.ml"

  | 3 ->
# 54 "lexer.mll"
                   ( 
                     comment lexbuf
                   )
# 227 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

and read_string buf lexbuf =
   __ocaml_lex_read_string_rec buf lexbuf 6
and __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 59 "lexer.mll"
              ( STR (Buffer.contents buf) )
# 239 "lexer.ml"

  | 1 ->
# 61 "lexer.mll"
               ( Buffer.add_char buf '"'; read_string buf lexbuf   )
# 244 "lexer.ml"

  | 2 ->
# 62 "lexer.mll"
               ( STR (Buffer.contents buf)  )
# 249 "lexer.ml"

  | 3 ->
# 63 "lexer.mll"
              ( Buffer.add_char buf '\012'; read_string buf lexbuf )
# 254 "lexer.ml"

  | 4 ->
# 64 "lexer.mll"
              ( Buffer.add_char buf '\n'; read_string buf lexbuf )
# 259 "lexer.ml"

  | 5 ->
# 65 "lexer.mll"
              ( Buffer.add_char buf '\r'; read_string buf lexbuf )
# 264 "lexer.ml"

  | 6 ->
# 66 "lexer.mll"
              ( Buffer.add_char buf '\t'; read_string buf lexbuf )
# 269 "lexer.ml"

  | 7 ->
# 68 "lexer.mll"
    ( Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    )
# 276 "lexer.ml"

  | 8 ->
# 71 "lexer.mll"
      ( raise (RUN_Error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) )
# 281 "lexer.ml"

  | 9 ->
# 72 "lexer.mll"
        ( raise (RUN_Error ("String is not terminated")) )
# 286 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state

and token lexbuf =
   __ocaml_lex_token_rec lexbuf 17
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 75 "lexer.mll"
                  ( 

                       token lexbuf(*consommer les delimiteurs, renvoyer lexbuf suivant*)
                    )
# 301 "lexer.ml"

  | 1 ->
let
# 79 "lexer.mll"
              id
# 307 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 79 "lexer.mll"
                           ( IDCLASS id )
# 311 "lexer.ml"

  | 2 ->
let
# 80 "lexer.mll"
              id
# 317 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 81 "lexer.mll"
    (

        try 
            Hashtbl.find keyword_table id
        with Not_found -> ID id    
    )
# 326 "lexer.ml"

  | 3 ->
# 87 "lexer.mll"
                  ( next_line lexbuf; token lexbuf)
# 331 "lexer.ml"

  | 4 ->
let
# 88 "lexer.mll"
               lxm
# 337 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 88 "lexer.mll"
                   ( CSTE(int_of_string lxm) )
# 341 "lexer.ml"

  | 5 ->
# 89 "lexer.mll"
                  ( comment lexbuf )
# 346 "lexer.ml"

  | 6 ->
# 90 "lexer.mll"
                  ( read_string (Buffer.create 17) lexbuf )
# 351 "lexer.ml"

  | 7 ->
# 91 "lexer.mll"
                  ( PLUS )
# 356 "lexer.ml"

  | 8 ->
# 92 "lexer.mll"
                  ( MINUS )
# 361 "lexer.ml"

  | 9 ->
# 93 "lexer.mll"
                  ( CONCATE )
# 366 "lexer.ml"

  | 10 ->
# 94 "lexer.mll"
                  ( TIMES )
# 371 "lexer.ml"

  | 11 ->
# 95 "lexer.mll"
                  ( DIV )
# 376 "lexer.ml"

  | 12 ->
# 96 "lexer.mll"
                  ( LPAREN )
# 381 "lexer.ml"

  | 13 ->
# 97 "lexer.mll"
                  ( RPAREN )
# 386 "lexer.ml"

  | 14 ->
# 98 "lexer.mll"
                  (LCBR)
# 391 "lexer.ml"

  | 15 ->
# 99 "lexer.mll"
                  (DOT)
# 396 "lexer.ml"

  | 16 ->
# 100 "lexer.mll"
                  (RCBR)
# 401 "lexer.ml"

  | 17 ->
# 101 "lexer.mll"
                  ( SEMICOLON )
# 406 "lexer.ml"

  | 18 ->
# 102 "lexer.mll"
                  ( COLON )
# 411 "lexer.ml"

  | 19 ->
# 103 "lexer.mll"
                  ( ASSIGN )
# 416 "lexer.ml"

  | 20 ->
# 104 "lexer.mll"
            ( RELOP (Ast.Lt) )
# 421 "lexer.ml"

  | 21 ->
# 105 "lexer.mll"
                  ( RELOP (Ast.Le) )
# 426 "lexer.ml"

  | 22 ->
# 106 "lexer.mll"
                  ( RELOP (Ast.Gt) )
# 431 "lexer.ml"

  | 23 ->
# 107 "lexer.mll"
                  ( RELOP (Ast.Ge) )
# 436 "lexer.ml"

  | 24 ->
# 108 "lexer.mll"
                  ( RELOP (Ast.Eq) )
# 441 "lexer.ml"

  | 25 ->
# 109 "lexer.mll"
                  ( RELOP (Ast.Neq) )
# 446 "lexer.ml"

  | 26 ->
# 110 "lexer.mll"
                  ( EOF )
# 451 "lexer.ml"

  | 27 ->
let
# 111 "lexer.mll"
        lxm
# 457 "lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 111 "lexer.mll"
                  ( (* dans ce cas il sagirait dun charactere non defini
                      *)
                     print_endline
                       ("undefined character: " ^ (String.make 1 lxm));
                     token lexbuf
                   )
# 466 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;
