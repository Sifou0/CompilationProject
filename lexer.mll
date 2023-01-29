{
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
      "begin", BEGIN;
      "end", END;
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
}


let lettre = ['A'-'Z' 'a'-'z']
let Maj = ['A'-'Z']
let Mij = ['a'-'b']
let chiffre = ['0'-'9']
let LC = ( chiffre | lettre )


(*le rule de comment ,NB rule est un mot propre a menhir comment est le nom de la regle*)
(* rule de comment (commentaire) *)
rule 
 comment = parse
             "*/" { 
                    token lexbuf
                  }
  | '\n'           { 
                     new_line lexbuf; comment lexbuf
                   }
  | eof            { 
                     raise (MISC_Error "unclosed comment")
                   }
  | _              { 
                     comment lexbuf
                   }
and read_string buf =
  parse
  | '"'       { STR (Buffer.contents buf) }

   | '\\' '"'  { Buffer.add_char buf '"'; read_string buf lexbuf   }
  | '\\' '\\'  { STR (Buffer.contents buf)  }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (RUN_Error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (RUN_Error ("String is not terminated")) }
  and
token = parse
 |[' ''\t''\r']+  { 

                       token lexbuf(*consommer les delimiteurs, renvoyer lexbuf suivant*)
                    } 
 |Maj LC * as id           { IDCLASS id } 
 |Mij LC * as id
    {

        try 
            Hashtbl.find keyword_table id
        with Not_found -> ID id    
    }              
 | '\n'           { next_line lexbuf; token lexbuf}
 | chiffre+ as lxm { CSTE(int_of_string lxm) }
 | "/*"           { comment lexbuf }
 | '"'            { read_string (Buffer.create 17) lexbuf }
 | '+'            { PLUS }
 | '-'            { MINUS }
 | '&'            { CONCATE }
 | '*'            { TIMES }
 | '/'            { DIV }
 | '('            { LPAREN }
 | ')'            { RPAREN } 
 | ']'            {RBRACKET}
 | '['            {LBRACKET}
 | '{'            {LCBR}
 | '.'            {DOT}        
 | '}'            {RCBR}
 | ';'            { SEMICOLON }
 | ':'            { COLON }
 | ":="           { ASSIGN }
 | "<"		    { RELOP (Ast.Lt) }
 | "<="           { RELOP (Ast.Le) }
 | ">"            { RELOP (Ast.Gt) }
 | ">="           { RELOP (Ast.Ge) }
 | "="            { RELOP (Ast.Eq) }
 | "<>"           { RELOP (Ast.Neq) }
 | eof            { EOF }
 | _ as lxm       { (* dans ce cas il sagirait dun charactere non defini
                      *)
                     print_endline
                       ("undefined character: " ^ (String.make 1 lxm));
                     token lexbuf
                   }                    

