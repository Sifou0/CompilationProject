type opComp =
Eq | Neq | Lt | Le | Gt | Ge

type prog = {
  classes : class_def list;
  objects : object_def list;
}

and class_def = {
  name : string;
  params : param_def list;
  attributs : param_def list;
  methods : method_def list;
  superclass : string option;
  constructor : block
}


and param_def = {
  name : string;
  class_type : string;
}

and object_def = {
  name : string;
  block_o : block;
  params : param_def list;
}

and block = {
  declarations : param_def list;
  instructions : instruction list;
}

and method_def = {
  name : string;
  params : param_def list;
  return_type : string;
  content : block;
  is_override : bool;
}

and instruction = 
    Exp of string (**)
  | Block of block
  | Aff of string*string (**)
  | Ite of expression*instruction*instruction (*if then else*)
  | Return

and ident = (*ou est la data*)
    Id of string
  | This of ident (* this.id *)
  | Super of ident (* super.id *)
  | Result

and expression = (*A compléter*)
    Ident of ident
  | IntCste of int
  | StringCste of string
  | Exp of expression
  | Cast of string*expression (* (Integer) x *)
  | NewInstance of string*expression list (* new Point(1,2)*)
  | Access of ident*string
  | Unary of expression

  

exception VC_Error of string
exception RUN_Error of string
exception MISC_Error of string
