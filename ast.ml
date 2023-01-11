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
  constructor : block;
}

and param_def = {
  name : string;
  class_type : string;
}

and object_def = {
  name : string;
  block_o : block;
  content : block;
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
    Exp of expression (**)
  | Block of block
  | Aff of ident*expression
  | Ite of expression*instruction*instruction
  | Return

and ident = (*ou est la data*)
    Id of string 
  | This of string
  | Super of string
  | Local of string
  | Result

and expression = (*A compléter*)
    Ident of ident
  | IntCste of int
  | StringCste of string
  | Exp of expression
  | Cast of string*expression (* (Integer) x *)
  | NewInstance of string*expression list (* new Point(1,2)*)
  | Access of ident*string (* x.id *)
  | Unary of expression
  | Plus of expression*expression 
  | Minus of expression*expression
  | Times of expression*expression
  | Concate of string 

and instructions = instruction list 


exception VC_Error of string
exception RUN_Error of string
exception MISC_Error of string
