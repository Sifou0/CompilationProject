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
      "return" , RETURN
    ]
}


let lettre = ['A'-'Z' 'a'-'z']
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
and 
token = parse
    lettre LC * as id
    {

        try 
            Hashtbl.find keyword_table id
        with Not_found -> ID id    
    }  
 |[' ''\t''\r']+  { 

                       token lexbuf(*consommer les delimiteurs, renvoyer lexbuf suivant*)
                    } 
 | '\n'           { next_line lexbuf; token lexbuf}
 | chiffre+ as lxm { CSTE(int_of_string lxm) }
 | "/*"           { comment lexbuf }
 | '+'            { PLUS }
 | '-'            { MINUS }
 | '*'            { TIMES }
 | '/'            { DIV }
 | '('            { LPAREN }
 | ')'            { RPAREN } 
 | ']'            {RBRACKET}
 | '['            {LBRACKET}
 | '{'            {LCBR}
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

