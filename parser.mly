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

%type <expression> expr

expr:
    x = ID { Ident(Id(x)) }
|   THIS x = expr { Ident(This(x)) }
|   SUPER x = expr { Ident(Super(x)) }
|   
%type <prog> prog


%start <Ast.prog> prog
%%

prog: ld = prog {   }
