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
%token THIS SUPER RESULT


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

param : n = ID COLON typevar = ID { {
    name = n;
    class_type = typevar;
} } 

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
        declarations = lp;
        instruction = instrs;
    }
}

ident = 
     THIS DOT x = ID { This(x) } 
    |SUPER DOT x = ID { Super(x) }
    |Local DOT x = ID { Local(x) }


expression = 
     id = ident { Ident(id) }   
    | n = CSTE { IntCste(n) }
    | a =  expression PLUS  b = expression { Plus(a,b) }   
    | a = expression MINUS b = expression { Minus(a,b) }
    | a = expression Times b = expression { Times(a,b) }
    | id = ident DOT n = ID { Access(id , n) }
    | NEW id = ID LPAREN le = list(expression) RPAREN   { NewInstance(id , le) } 
    | CAST LPAREN tipe = ID RPAREN expression { Cast(tipe , expression) }
    | num = int  {IntCste(num)}
    | str = string { StringCste(str) }

instruction = 
    n = expression { Exp(n) } 
  | bl = bloc { Block(bl) }
  | n = ID ASSIGN r = ID {Aff(n,r)}
  | IF si=expression THEN alors = instruction ELSE sinon = instruction {Ite(si,alors,sinon)} 

