open Ast

let string_of_relop (op : Ast.opComp) = 
  match op with
    Eq -> "="
  | Neq ->"<>"
  | Lt  -> "<"
  | Le  -> "<="
  | Gt  -> ">"
  | Ge  -> ">="
;;
  let string_of_ident (id : Ast.ident) = 
    match id with
      This -> "This"
    | Super ->"Super"
    | Local s  -> print_string s
    | Result  -> "Result"  
  ;;
  let rec printExpression exp = 
    match exp with
      Id id -> print_string id
    | Cste cst -> print_int cst 
    | Plus(a,b) -> print_string"[ ";printExpression a ; print_string " + "; printExpression b ; print_string " ]"
    | Minus(a, b)-> print_string"[ ";printExpression a ; print_string " - "; printExpression b ; print_string " ]"
    | Times(a,b) -> print_string"[ ";printExpression a ; print_string " * "; printExpression b ; print_string " ]"
    | Div(a,b) -> print_string"[ ";printExpression a ; print_string " / "; printExpression b ; print_string " ]"
    | Comp(op, a,b) ->  print_string "[ "; printExpression a; print_string (string_of_relop op); printExpression b; print_string " ]"
    | Concate(a, b) -> print_string "Concatenate( " ; printExpression a; print_string " , " ;printExpression b ; print_string " )"
    |NewInstance (g, d) -> print_string "NewInstance( "; print_string g; print_string " , args :"
      List.iter (fun x -> print_string " ( "; printExpression x; print_string " )") d; print_string " )"
    |Access(a, id) -> print_string"( " ; printExpression a ; print_string "." ; printExpression id
    |Cast(a , b) -> print_string"Cast " ; print_string a ; printExpression b
    |StringCste a -> print_string "StringCste( ";print_string a ; print_string " )"
    |Ident idt -> string_of_ident idt 
  ;;
let rec printInstruction instr =
  match instr with
    Exp exp ->print_string "Expression " ; printExpression exp
  | Aff (id , exp) -> print_string "( ";string_of_ident id ; print_string " = "; printExpression exp ; print_string " )" 
  | Ite(exp ,inst1 ,inst2) -> print_string "If( ";printExpression exp ;print_string" )"; print_string "Then "; print_string " { ";printInstruction inst1 ; print_string " }";print_string "Else{ "; printInstruction inst2 ; print_string " }"
  | Return -> print_string"Return"
;;
let rec printBlock bl =
  print_string "{ ";
  List.iter printDecl bl.declarations;
  List.iter printInstruction bl.instructions;
  print_string " }"
;;  

let rec printOption f x = 
  match x with
      None -> ()
     | Some u -> f u
;;

let printDecl decl = 
  print_string decl.class_type ; print_string " ";
  if(decl.is_auto)print_string "auto";
    print_string decl.name ;print_string " ";   
;;
let rec printMethod meth = 
  print_string "def ";
  if(meth.is_override) print_string"Override";
  print_string meth.name_meth;
  print_string "( ";
  List.iter printDecl meth.params ;
  print_string " ) : is ";
  printBlock meth.content;
;;

let rec printBlockClass bl_cl=
print_string " { ";
List.iter printDecl bl_cl.declarations;
List.iter printInstruction bl_cl.instructions;
print_string " } ";

let rec printObjet ob =
  print_string "object ";
  print_string obj.name_class ;
  print_String " ";
  printOption printBlock ob.constructor;
  printBlockClass ob.content;

let rec printClass cl =
 if cl.is_class = true 
  then
   print_string "Class ";
   print_string cl.name_class;
  print_string " ( ";
  List.iter printDecl cl.params;
  print_string " ) ";
  (match cl.superclass with
    |None -> print_string " "
    |Some s -> print_string " extends "; print_string s
  );
  printOption printBlock cl.constructor ;
  printBlockClass cl.content;
else
  printObjet cl
;;

let rec printAll lc i =
    List.iter printClass lc;
    printInstr i;
    print_newline ()