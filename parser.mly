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

%token DOT
%token <string>  IDCLASS
%token COLON /*:*/
%token LBRACKET /*[*/
%token RBRACKET /*]*/
%token LCBR  /*{*/
%token RCBR  /*}*/
%token OVERRIDE

%token IF THEN ELSE
%token THIS SUPER RESULT




%type <Ast.expression> expression //envoiMsg target
%type <Ast.class_def> classe
%type <Ast.declaration> declaration

%type <Ast.block> block
%type <Ast.block_class> block_class
%type <Ast.method_def> methode
%type <Ast.ident> ident

%type <Ast.instruction> instruction
//%type <Ast.method_def>methode
/*Les precedences*/

%token UMINUS
%token EOF
%left PLUS MINUS        
%left TIMES DIV
%right UMINUS 
%left RELOP
%start <Ast.prog> prog

%%

prog: ld = list(classe) bl = block EOF { 
    {
             classes = ld ;
             block = bl ;
    } 
                                         
}

declaration :
    VAR a = boption(AUTO) n = ID COLON typevar = ID {
        {
            name = n;
            class_type = typevar;
            is_auto = a;
        }
    }                                                                                           

classe : 
    c = boption(CLASS) o = boption(OBJECT) n = IDCLASS LPAREN lp = list(declaration) s = option(extends) b = option(block) IS LBRACKET bb = block_class
    {
        {
            name = n;
            is_class = c;
            is_object = o;
            params = lp;
            superclass = s;
            constructor = b;
            content = bb;
        }
    }

extends : EXTENDS c = IDCLASS { c }

block : LCBR ld = list(declaration)  instrs = list(instruction)    RCBR {
    {
        declarations = ld;
        instructions = instrs;

    }
}

block_class : LCBR ld = list(declaration) lm = list(methode) RCBR {
   {  declarations = ld ;
    methodes = lm ;
    }
}

methode : DEF n = ID LPAREN lp = list(declaration) RPAREN COLON cls = IDCLASS  bl = block
            {
                {
                    name = n;
                    params = lp;
                    return_type = Some(cls);
                    content = bl ;
                    is_override = false;
                }

            }
        |   DEF n = ID LPAREN lp = list(declaration) RPAREN bl = block 
            {
                {
                    name = n;
                    params = lp;
                    return_type = None ;
                    content = bl;
                    is_override = false ;
                }
            }
        | DEF OVERRIDE n = ID LPAREN lp = list(declaration) RPAREN bl = block 
            {
                {
                    name = n;
                    params = lp;
                    return_type = None;
                    content = bl;
                    is_override = true;
                }
            }
        | DEF OVERRIDE n = ID LPAREN lp = list(declaration) RPAREN COLON cls = IDCLASS  bl = block 
            {
                {
                    name = n;
                    params = lp;
                    return_type = Some(cls) ;
                    content = bl;
                    is_override = true;
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
    | a = expression DIV b = expression {Div(a,b)}
    | a = expression CONCATE b = expression { Concate(a,b) }
    | UMINUS e = expression { Unary(e) }
    | id = ident DOT n = ID { Access(id , n) }
    | NEW id = IDCLASS LPAREN le = list(expression) RPAREN   { NewInstance(id , le) }
    | id = IDCLASS LPAREN le = list(expression) RPAREN   { NewInstance(id , le) }
    | LPAREN tipe = ID e = expression RPAREN { Cast(tipe , e) }
    | LPAREN e =  expression RPAREN  { e }
    | a = expression op = RELOP b = expression {Compo(op,a,b)}
  //  | a = envoiMsg  { a }


//    envoiMsg : 
//             | s = STR {StringCste s}
//             | t = target { t }

//     target :
//              x = ID              { Id x }
//             |o = envoiMsg DOT s = ID  { CallElement(e, Id s) }
//           //  |o = IDCLASS DOT c = ID   { CallElement(o , Id c) }
//             |LPAREN e = expression RPAREN { e }

instruction :
    n = expression SEMICOLON { Exp(n) } 
 // | ld = list(declaration)  li=list(instruction)  { Block(ld,li) }
  | n = ident ASSIGN r = expression {Aff(n,r)}
  | IF si=expression THEN alors = instruction ELSE sinon = instruction {Ite(si,alors,sinon)} 

