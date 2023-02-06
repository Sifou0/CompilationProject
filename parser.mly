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
%token <string> STR
%token CONCATE
%token DEF
%token EXTENDS
%token NEW
%token OBJECT
%token COMMA

%token DOT
%token <string>  IDCLASS
%token COLON /*:*/
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
%type <Ast.declaration> factoredVarParam
%type <Ast.declaration list> params
%type <Ast.instruction> instruction
//%type <Ast.method_def>methode
/*Les precedences*/


%token UMINUS
%token EOF
%left PLUS MINUS        
%left TIMES DIV
%left RELOP
%left DOT
//%right LPAREN
%left CONCATE
%right UMINUS 
%start <Ast.prog> prog

%%

prog: 
    ld = list(classe) bl = block EOF { 
        {
                classes = ld ;
                block = bl ;
        }                                      
    }

declaration :
     v = VAR a = boption(AUTO) n = ID COLON typevar = ID {
        {
            name = n;
            class_type = typevar;
            is_auto = a;
            is_var = true;
        }
    }                                                                                           

classe : 
    c = boption(CLASS) o = boption(OBJECT) n = IDCLASS lp = option(params) s = option(extends) b = option(block) IS bb = block_class
    {
        {
            name_class = n;
            is_class = c;
            is_object = o;
            params = lp;
            superclass = s;
            constructor = b;
            content = bb;
        }
    }

extends : EXTENDS c = IDCLASS { c }

block : LCBR instrs = list(instruction) ld = separated_list(SEMICOLON, declaration) RCBR {
    {
        instructions = instrs;
        declarations = ld;
    }
}

block_class : LCBR ld = separated_list(SEMICOLON,declaration) lm = separated_list(SEMICOLON,methode) RCBR {
   {  declarations = ld ;
    methodes = lm ;
    }
}

        
methode : 
    DEF o = boption(OVERRIDE) n = ID LPAREN lp =params RPAREN  r = option(returned_type) IS b = block
    {
        {
            is_override = o;
            name_meth = n;
            params = lp;
            return_type = r;
            content_methode = b;
        }
    }
    | DEF o = boption(OVERRIDE) n = ID LPAREN lp =params RPAREN  r = returned_type ASSIGN b = block
    {
        {
            is_override = o;
            name_meth = n;
            params = lp ;
            return_type = Some r;
            content_methode = b;
        }
    }

returned_type:
    COLON c = IDCLASS { c } 

ident:
    | n = ID { Local n }
    | THIS { This } 
    | SUPER { Super }
    | RESULT { Result }
 
factoredVarParam:
    id = ID r = returned_type
    {
        {
            name = id ; class_type = r ;is_auto = false;is_var = false 
        } 
    } 


params:
    LPAREN p = separated_list(COMMA, factoredVarParam) RPAREN { p } 


expression:    
     n = CSTE { IntCste n }
    | s = STR { StringCste s }
    | id = ident { Ident(id) }
    | a =  expression PLUS  b = expression %prec PLUS { Plus(a,b) }   
    | a = expression MINUS b = expression %prec MINUS { Minus(a,b) }
    | a = expression TIMES b = expression %prec TIMES { Times(a,b) }
    | a = expression DIV b = expression %prec DIV {Div(a,b)}
    | a = expression CONCATE b = expression { Concate(a,b) }
    | UMINUS e = expression { Unary(e) }
    | NEW id = IDCLASS LPAREN le = list(expression) RPAREN   { NewInstance(id , le) }
    | id = IDCLASS LPAREN le = list(expression) RPAREN   { NewInstance(id , le) }
    | LPAREN tipe = ID e = expression RPAREN { Cast(tipe , e) }
    | LPAREN e =  expression RPAREN  { e }
    | a = expression op = RELOP b = expression {Compo(op,a,b)}
    | a = expression DOT i = ident { Access(a,i) }
  //  | a = expression DOT n = ident LPAREN le = list(expression) RPAREN { EnvoiMsg(a,n,le) }


// envoiMsg : 
//     | s = STR {StringCste s}
//     | t = target { t }
//     | 

// target :
//     x = ID              { Id x }
//     |o = envoiMsg DOT s = ID  { CallElement(e, Id s) }
//     |o = IDCLASS DOT c = ID   { CallElement(o , Id c) }
//     |LPAREN e = expression RPAREN { e }

instruction :
    n = expression SEMICOLON{ Exp(n) } 
 // | ld = list(declaration)  li=list(instruction)  { Block(ld,li) }
  | n = ident ASSIGN r = expression SEMICOLON{Aff(n,r)}
  | IF si=expression THEN alors = instruction ELSE sinon = instruction SEMICOLON {Ite(si,alors,sinon)}
  | RETURN SEMICOLON { Return }

