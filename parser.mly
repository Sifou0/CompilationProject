%{
    open Ast
%}

%token <string> ID
%token <int> CSTE
%token <Ast.opComp> RELOP
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN SEMICOLON
%token ASSIGN
%token RETURN
%token CLASS
%token IS
%token VAR
%token AUTO
%token STR
%token CONCATE
%token DEF
%token EXTENDS
%token NEW
%token OBJECT
%token STRING
%token DOT
%token CAST
%token COLON /*:*/
%token LBRACKET /*[*/
%token RBRACKET /*]*/
%token LCBR  /*{*/
%token RCBR  /*}*/
%token OVERRIDE

%token IF THEN ELSE BEGIN END
%token THIS SUPER RESULT


/*Les precedences*/
%token UMINUS
%token EOF
%left PLUS MINUS        
%left TIMES   
%right UMINUS 

%type <Ast.expression> expression
%type <Ast.class_def> classe
%type <Ast.declaration> declaration

%type <Ast.block> block
//%type <Ast.method_def> methode
%type <Ast.ident> ident

%type <Ast.instruction> instruction




%start <Ast.prog> prog

%%

prog: ld =list(classe) bl = block EOF { 
    {
             classes = ld ;
             block = bl ;
    } 
                                         
 }

declaration : VAR n = ID COLON typevar = ID { {
    name = n;
    class_type = typevar;
} } 

classe : CLASS n = ID LPAREN lp = list(declaration)  RPAREN IS LBRACKET bl = block RBRACKET { {
                                                                name = n;
                                                                params = lp;
                                                                superclass = None;
                                                                content = bl;
                                                                constructor = None;
                                                                is_class = true;
                                                                            } 
                                                             }

        |CLASS n = ID LPAREN ld = list(declaration)  RPAREN EXTENDS spr_class = expression  IS LBRACKET bl = block RBRACKET { 
                                                            {
                                                                name = n;
                                                                params = ld;
                                                                superclass = Some(spr_class);
                                                                constructor = None;
                                                                content = bl;
                                                                is_class = true;
                                                                            } 
                                                            }                  
         |CLASS n = ID LPAREN ld = list(declaration)  RPAREN EXTENDS spr_class = expression ID LBRACKET construc = block RBRACKET IS LBRACKET bl = block RBRACKET { 
                                                            {
                                                                name = n;
                                                                params = ld;
                                                                superclass = Some(spr_class);
                                                                constructor = Some(construc);
                                                                content = bl;
                                                                is_class = true;
                                                            } 
                                                        }   

           |CLASS n = ID LPAREN lp = list(declaration)   RPAREN LPAREN RPAREN LBRACKET construc = block RBRACKET  IS LBRACKET bl = block RBRACKET { {
                                                                name = n;
                                                                params = lp;
                                                                constructor = Some(construc);
                                                                content = bl;
                                                                is_class = true;
                                                                superclass = None;
                                                                } 
                                                             }                                                                                           

block : LCBR ld = list(declaration)  instrs = list(instruction)   RCBR {
    {
        declarations = ld;
        instructions = instrs;
    }
}

ident : 
     THIS DOT x = ID { This(x) } 
    |SUPER DOT x = ID { Super(x) }


expression :    
     n = CSTE { IntCste n }
    | s = ID { StringCste s }
    | id = ident { Ident(id) }
    | a =  expression PLUS  b = expression { Plus(a,b) }   
    | a = expression MINUS b = expression { Minus(a,b) }
    | a = expression TIMES b = expression { Times(a,b) }
    | UMINUS e = expression { Unary(e) }
    | id = ident DOT n = ID { Access(id , n) }
    | NEW id = ID LPAREN le = list(expression) RPAREN   { NewInstance(id , le) }
    | id = ID LPAREN le = list(expression) RPAREN   { NewInstance(id , le) }
    | LPAREN tipe = ID e = expression RPAREN { Cast(tipe , e) }
    | LPAREN e =  expression RPAREN  { e }
    

instruction :
    n = expression { Exp(n) } 
 // | ld = list(declaration)  li=list(instruction)  { Block(ld,li) }
  | n = ident ASSIGN r = expression {Aff(n,r)}
  | IF si=expression THEN alors = instruction ELSE sinon = instruction {Ite(si,alors,sinon)} 

