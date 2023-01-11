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
%token DEF
%token EXTENDS
%token NEW
%token OBJECT
%token LPAREN
%token COLON /*:*/
%token RPAREN
%token LBRACKET /*[*/
%token RBRACKET /*]*/
%token LCBR  /*{*/
%token RCBR  /*}*/
%token OVERRIDE
%token PLUS
%token IF THEN ELSE BEGIN END


/*Les precedences*/
%token UMINUS
%token EOF
%right ELSE
%left PLUS MINUS        
%left TIMES DIV   

%type <Ast.prog> prog
%type <Ast.expression> expr
%type <Ast.class_def> class
%type <Ast.param_def> param
%type <Ast.object_def> object
%type <Ast.block> bloc
%type <Ast.method_def> method
%type <Ast.ident> id
%type <Ast.expression> expression
%type <Ast.instruction> instruction
%type <Ast.instructions> instructions



%start <Ast.prog> prog

%%

prog: ld =list(class) bl = bloc EOF { 
    {
             classes = ld ;
             objects = bl ;
    } 
                                         
 }

classe : CLASS n = name LPAREN lp = list(param)  RPAREN    IS bl = bloc { {
                                                                name = n;
                                                                params = lp;
                                                                block_c = bl;
                                                                            } 
                                                             }

        |CLASS n = name LPAREN lp = list(param)  RPAREN  EXTENDS spr_class = class.name  IS bl = bloc { 
                                                            {
                                                                name = n;
                                                                params = lp;
                                                                block_c = bl;
                                                                superclass = superclass;
                                                                            } 
                                                            }                                          
bloc : LCBR lp = list(param)  instrs = instructions  RCBR {
    {
        declarations = param;
        instruction = instrs;
    }
}

ident = 
     THIS x = ID { This(x) } 
    |SUPER x = ID { Super(x) }
    |Local 


instruction = 
    n = expression { Exp(n) } 
  | bl = bloc { Block(bl) }
  | n = ID ASSIGN r = ID {Aff(n,r)}
  | IF si=expression THEN alors = instruction ELSE sinon = instruction {Ite(si,alors,sinon)} 

expression = 
     id = ident { Ident(id) }   
    |n = int { IntCste(n) }
    |
    |