type opComp =
Eq | Neq | Lt | Le | Gt | Ge

type prog = {
  classes : class_def list;
  block : block
}

and class_def = {
  name : string;
  is_class : bool;
  is_object : bool;
  params : declaration list;
  (* attributs : declaration list; *)
  superclass : string option;
  constructor : block option;
  content : block_class;

}

and declaration = {
  name : string;
  class_type : string;
  is_auto : bool ;
}

(* and object_def = {
  name : string;
  block_o : block;
  content : block;
} *)
and block_class ={
  declarations : declaration list;
  methodes : method_def list;

} 
and block = {
  (* methods : method_def list; *)
  declarations : declaration list;
  instructions : instruction list;

}

and method_def = {
  name : string;
  params : declaration list;
  return_type : string option;
  content : block;
  is_override : bool;
}

and instruction = 
    Exp of expression (**)
  (* | Block of declaration list * instruction list *)
  | Aff of ident*expression
  | Ite of expression*instruction*instruction
  | Return

and ident = (*ou est la data*)
  | This
  | Super
  | Local of string
  | Result

and expression = (*A compléter*)
    Ident of ident
  | IntCste of int
  | StringCste of string
  | Cast of string*expression (* (Integer) x *)
  | NewInstance of string*expression list (* new Point(1,2)*)
  | Access of expression*ident (* x.id *)
  | Unary of expression
  | Plus of expression*expression 
  | Minus of expression*expression
  | Times of expression*expression
  | Div of expression*expression
  | Concate of expression*expression
  | Compo of opComp*expression*expression 
  | CallElement of expression*expression
  | EnvoiMsg of expression*ident*expression list


exception VC_Error of string
exception RUN_Error of string
exception MISC_Error of string
