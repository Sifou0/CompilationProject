open Ast

(* Récupère la définition d'une classe depuis une chaine de caractère *)
let rec getStrOfClass str ld =
  match ld with
  | [] -> raise (VC_Error ("La classe \"" ^ str ^ "\" n'est pas définie."))
  | d::s -> if (String.equal str d.name_class) then d else getStrOfClass str s
;;

(* Retourne vrai si la classe avec le nom "str" existe *)
let rec isClassNameExists str ld =
  List.exists (fun x -> String.equal x.name_class str) ld
;;

(* retourne vrai si la premiere lettre est en majuscule *)
let isFirstLetterUpper str = 
  let i = int_of_char str.[0]
  in 65 <= i && i <= 90
;;

let rec verifExistOfClass str l =
  match l with
  | [] -> false
  | x::s -> if (String.equal x.name_class str) then true else (verifExistOfClass str s)
;;

(* Permet de vérifier que la super classe existe *)
let rec verifExistOfSuperClass x l =
  match x.superclass with
  | None -> true
  | Some s -> List.exists (fun x -> x.name_class = s) l
;;


let verifyInHeritance l =
  List.fold_left (
    fun acc x -> acc && (
      if (verifExistOfSuperClass x l) then true
      else raise (VC_Error ("La super classe de \"" ^ x.name_class ^ "\" n'est pas définie.")) ))
;;

let rec countNameClass str l =
  match l with
  | [] -> 0
  | x::s -> if (String.equal x.name_class str) then (countNameClass str s) + 1 else (countNameClass str s)
;;

let verifyDoubleNameOfClass l =
  List.fold_left (fun acc x -> acc && (
    if ((countNameClass x.name_class l) < 2) then true else raise (VC_Error ("La classe \"" ^ x.name_class ^ "\" est définie plusieurs fois.")))) true l
;;


let getOptString s =
  match s with
  | None -> ""
  | Some str -> str
;;

(* vérifier si un mot clé "str" est utilisé dans l'expression *)
let rec verifyExistanceOfExpKey str e =
  match e with
  | Ident s -> false
  | StringCste s -> false
  | IntCste s -> false
  | Plus(g,d) -> (verifyExistanceOfExpKey str g) || (verifyExistanceOfExpKey str d)
  | Minus(g,d) -> (verifyExistanceOfExpKey str g) || (verifyExistanceOfExpKey str d)
  | Times(g,d) -> (verifyExistanceOfExpKey str g) || (verifyExistanceOfExpKey str d)
  | Div(g,d) -> (verifyExistanceOfExpKey str g) || (verifyExistanceOfExpKey str d)
  | Compo(_,g,d) -> (verifyExistanceOfExpKey str g) || (verifyExistanceOfExpKey str d)
  | Cast(s,e) -> (String.equal str s) || (verifyExistanceOfExpKey str e)
  | NewInstance(s,le) -> (String.equal str s) || (List.fold_left (fun acc x -> acc && (verifyExistanceOfExpKey str x)) false le)
  | Concate(g,d) -> (verifyExistanceOfExpKey str g) || (verifyExistanceOfExpKey str d)
  | Access(e,i) -> verifyExistanceOfExpKey str e
  (* | CallElement(g,d) -> (verifyExistanceOfExpKey str g) || (verifyExistanceOfExpKey str d) *)
  | Unary _ -> false

  let rec verifyExistanceOfInstrKey str i =
    match i with
    | Exp e -> verifyExistanceOfExpKey str e;
    | Aff(e1,e2) ->  verifyExistanceOfExpKey str e2

    | Ite(si, sinon, alors) -> (verifyExistanceOfExpKey str si) || (verifyExistanceOfInstrKey str alors) || (verifyExistanceOfInstrKey str sinon)
    | Return -> false
  ;;
  
  (* vérifier si le mot-clé "this" est utilisé dans le bloc principal *)
let verifyUsageThis bloc =
  if (verifyExistanceOfInstrKey "this" bloc) then raise (VC_Error "L'utilisation du mot clé \"this\" n'est pas autorisé en dehors des définitions de classe.")
      else true
;;

let verifyUsageSuper bloc =
  if (verifyExistanceOfInstrKey "super" bloc) then raise (VC_Error "L'utilisation du mot clé \"super\" n'est pas autorisé en dehors des définitions de classe.")
      else true
;;



let rec getMethodesRedefind d ld =
  match d.superclass with
  | None -> []
  | Some str -> let c = getStrOfClass str ld
                in getMethodesRedefind c ld;;

(* retourne la liste des méthodes déclaré comme étant override *)
let getOverrideMethods c =
  List.find_all (fun x -> x.is_override = true) c.content.methodes
;;

(* Compare si deux listes de paramètres sont équivalentes *)
let compareLenParam lp1 lp2 = 
  if List.length lp1 = List.length lp2 then 
  let rec types l1 l2 = match l1 with
  | [] -> true
  | x::s -> match l2 with
    | [] -> true
    | y::t -> x.class_type = y.class_type && types s t
in types lp1 lp2
else false
;;

(* vérifie si la signature de la méthode "m" fait partie de la liste des methodes "lm" *)
let rec verifyExistanceMeth m lm =
  match lm with
  | [] -> false
  | meth::l -> if (
  (String.equal meth.name_meth m.name_meth) && 
  (String.equal (getOptString meth.return_type) (getOptString m.return_type)) && 
  (compareLenParam meth.params m.params)) then true else  verifyExistanceMeth m l
;;

(* vérifie que les méthodes définie comme "override" sont bien en train d'override une méthode parent *)
let verifyUsageOverride ld =
  let rec verifOverride l =
    match l with
    | [] -> true
    | c::s -> let lm = getMethodesRedefind c ld
              in let lmo = getOverrideMethods c
              in (List.fold_left (fun acc m -> acc && (
                if (verifyExistanceMeth m lm) then true
                else raise (VC_Error ("La méthode \"" ^ m.name_meth ^ "\" dans la classe \"" ^ c.name_class ^ "\" ne redéfinie aucune méthode."))
                )) true lmo) && verifOverride s
  in verifOverride ld
;;




(* vérifie la présence de cycle dans l'héritage *)
let verifyHeritage ld =
  let rec verifHeri l =
    match l with
    | [] -> true
    | d::s -> let rec verify c =
        if (String.equal c.name_class d.name_class) then
          raise (VC_Error ("La classe \"" ^ c.name_class ^ "\" forme un cycle dans son héritage."))
        else (
          match c.superclass with
          | None -> true
          | Some str -> verify (getStrOfClass str ld)
        )
      in match d.superclass with
        | None -> true && verifHeri s
        | Some str -> verify (getStrOfClass str ld)
  in verifHeri ld
;;

(* verifie la présence et l'unicité des constructeurs *)
(* let rec verifyYesConstructor ld =
  match ld with
  | [] -> true
  | c::s -> let nbConstructor = List.fold_left (fun acc x -> match x with |  constructor -> acc+1 | _ -> acc) 0 c.
    in if (nbConstructor == 0) then
      raise (VC_Error ("La classe \"" ^ c.name_class ^ "\" n'a pas de constructeur."))
    else if (nbConstructor > 1) then
      raise (VC_Error ("La classe \"" ^ c.name_class ^ "\" a plusieurs constructeurs."))
    else
      verifyYesConstructor s
;; *)

(* let getConstrFromClass c =
  c.constructor
;; *)




(* let rec compareLenParam lp1 lp2 =
  match lp1 with
  | [] ->
    begin
      match lp2 with
      | [] -> true
      | _ -> false
    end
  | p1::s1 ->
    begin
      match lp2 with
      | [] -> false
      | p2::s2 -> if ((p1.var == p2.var) && (List.length p1.declarations) == (List.length p2.declarations) && (String.equal p1.name p2.name)) then compareLenParam s1 s2 else false
    end
;; *)

(* Récupère le nom de la classe d'une instanciation ainsi que ces paramètres *)
let getInstanciationInfo i =
  match i with
  | NewInstance(str,l) -> (str,l)
  | _ -> raise (VC_Error ("getInstanciationInfo : argument incorrect."))
;;

(* Retourne le nombre de paramètres nécessaires pour instancier la classe "c" *)
let getNbParamsRequiered c = List.length c.params

(* let getNbParamsRequiered c =
  let rec gtnbparam acc lp =
    match lp with
    |[] ->acc
    |_::s  -> gtnbparam (acc+1) s
  in
  gtnbparam 0 c.params
;; *)


(* vérifie la cohérence du constructeur par rapport à l'en-tete de la classe (Son nom, le nombre et le type des arguments, l'appel au constructeur de classe héritée)
let verifierCoherenceConstr ld =
  let rec verifCoheConstr l = 
  match l with
  | [] -> true
  | c::s -> let constr = getConstrFromClass c;
      if (compareLenParam c.params constr.declarations) then (
        match constr.superClassConstrCall with
        | None -> (
          match c.superclass with 
          | None -> verifCoheConstr s 
          | Some str -> raise (VC_Error ("Le constructeur de la classe \"" ^ c.name_class ^ "\" ne fait pas appel au constructeur de la classe mère définie dans l'en-tete de la classe."))
        )
        | Some superConstrCall -> (
          match c.classSuper with 
          | None -> raise (VC_Error ("Le constructeur de la classe \"" ^ c.className ^ "\" fait appel au constructeur d'une classe mère non définie dans l'en-tete de la classe.")) 
          | Some str -> let superClassName, lArgs = getInstanciationInfo superConstrCall
            in if ((String.equal (getOptString c.classSuper) superClassName) && (List.length lArgs) == getNbParamsRequiered ( getStrOfClass superClassName ld)) then
              verifCoheConstr s
            else
            raise (VC_Error ("Dans la classe \"" ^ c.name_class ^ "\", l'appel au constructeur de la classe mère est incorrect."))
        )
      )
      else
        raise (VC_Error ("Les paramètres de la classe \"" ^ c.name_class ^ "\" et de son constructeur ne sont pas les mêmes."))
    
  in verifCoheConstr ld
;; *)

(* Vérifie si les noms des classes définies sont autorisés *)
let verifyNameClass ld =
  let res = List.exists (fun x -> (String.equal "String" x.name_class)) ld
  in if (res) then
    raise (VC_Error ("Impossible de définir une classe avec le nom \"String\""))
  else
    let res = List.exists (fun x -> (String.equal "Integer" x.name_class)) ld
    in if (res) then
      raise (VC_Error ("Impossible de définir une classe avec le nom \"Integer\""))
    else
      true
;;


let rec checkExpressionThisSuper (e : expression) : bool = 
  match e with
  | Ident ex -> (match ex with
    | This -> false
    | Super -> false
    | Local(_) -> true
    | Result -> true)
  | IntCste(i)-> true
  | StringCste(s)-> true
  | Cast(s,e1)-> checkExpressionThisSuper e1
  | NewInstance(s,le)-> List.for_all checkExpressionThisSuper le
  | Access(e1,i)-> checkExpressionThisSuper e1 && (match i with
    | This -> false
    | Super -> false
    | Local(_) -> true
    | Result -> true)
  | Unary(e1)-> checkExpressionThisSuper e1
  | Plus(e1,e2)-> checkExpressionThisSuper e1 && checkExpressionThisSuper e2
  | Minus(e1,e2)-> checkExpressionThisSuper e1 && checkExpressionThisSuper e2
  | Times(e1,e2)-> checkExpressionThisSuper e1 && checkExpressionThisSuper e2
  | Div(e1,e2)-> checkExpressionThisSuper e1 && checkExpressionThisSuper e2
  | Concate(e1,e2)-> checkExpressionThisSuper e1 && checkExpressionThisSuper e2
  | Compo(_,e1,e2)-> checkExpressionThisSuper e1 && checkExpressionThisSuper e2



let checkSuperThisInMain b =
  let rec inter l = 
  match l with
  | [] -> true
  | x::s ->
    (
      match x with
        | Exp(e) -> checkExpressionThisSuper e && inter s
        | Aff(e1,e) -> checkExpressionThisSuper e && checkExpressionThisSuper e1 && inter s
        | Ite(e,i1,i2) -> checkExpressionThisSuper e
        | Return -> true
    )
  in inter b.instructions
;;

let isUpclass c l = List.exists (fun x -> x = c.superclass) l;;
(* Integer et String ne font pas partie de la liste des classes autorisees donc pas besoin de verif dessus *)

let handleOptionMeth md l = match md.return_type with | None -> true | Some(a) -> List.exists (fun x -> x = a) l

 let checkMethode md l = 
  (handleOptionMeth md l || md.return_type = Some "Integer" || md.return_type = Some "String") &&
  let rec inter lp = 
    match lp with
    | [] -> true
    | x::s -> List.exists (fun n -> n = x.class_type) l && inter s
  in inter md.params
;;

 let getTypeOfIdent i cd =
  match i with
  | This -> cd.name_class
  | Super -> (match cd.superclass with | Some a -> a | None -> raise(VC_Error "ff"))
  | Result -> "Result"
  | Local x -> let d = List.find (fun a -> a.name = x) cd.content.declarations in d.class_type



let rec getTypeOfExp e cd = 
  match e with
  | Ident(ex) -> getTypeOfIdent ex cd
  | IntCste x-> "Integer"
  | StringCste x-> "String"
  | Cast(s,ex) -> s
  | NewInstance(s,le) -> s
  | Access(ex,i) -> getTypeOfIdent i cd
  | Unary(ex) -> getTypeOfExp ex cd
  | Plus(e1,e2) -> if((getTypeOfExp e1 cd) = (getTypeOfExp e2 cd) && (getTypeOfExp e1 cd = "Integer")) then (getTypeOfExp e1 cd) else raise(VC_Error("Expression mal formee"))
  | Minus(e1,e2) -> if((getTypeOfExp e1 cd) = (getTypeOfExp e2 cd) && (getTypeOfExp e1 cd = "Integer")) then (getTypeOfExp e1 cd)else raise(VC_Error("Expression mal formee"))
  | Times(e1,e2) -> if((getTypeOfExp e1 cd) = (getTypeOfExp e2 cd) && (getTypeOfExp e1 cd = "Integer")) then (getTypeOfExp e1 cd) else raise(VC_Error("Expression mal formee"))
  | Div(e1,e2) -> if((getTypeOfExp e1 cd) = (getTypeOfExp e2 cd) && (getTypeOfExp e1 cd = "Integer")) then (getTypeOfExp e1 cd) else raise(VC_Error("Expression mal formee"))
  | Concate(e1,e2) -> if((getTypeOfExp e1 cd) = (getTypeOfExp e2 cd) && (getTypeOfExp e1 cd = "String")) then (getTypeOfExp e1 cd) else raise(VC_Error("Expression mal formee"))
  | Compo(_,e1,e2) -> if((getTypeOfExp e1 cd) = (getTypeOfExp e2 cd) && (getTypeOfExp e1 cd = "Integer")) then (getTypeOfExp e1 cd) else raise(VC_Error("Expression mal formee"))
;; 

let checkVarNotResult b = 
  let rec inter l =
    match l with
    | [] -> true
    | x::s -> (x.name <> "result") && inter s
  in inter b.declarations
;;

let cycleHeritage c ht = 
  let rec inter cd acc =
    match cd.superclass with
    | None -> true
    | Some a -> if List.exists (fun x -> x = a) acc then false else inter (Hashtbl.find ht a) (acc@[a])
  in inter c []

let checkMethInClass m cd =
  let rec inter l = match l with
  | [] -> false
  | x::s -> if x.name_meth = m then true else inter s
in inter cd.content.methodes

let rec correctOverride md cd ht =
  if md.is_override then true else
    match cd.superclass with
    | None -> false
    | Some a -> let sp = (Hashtbl.find ht a) in (List.exists (fun x -> md.name_meth = x.name_meth) sp.content.methodes) || correctOverride md sp ht
  ;;

let rec checkInstr i ht cd = 
  match i with
  | Exp e -> let p = (match Hashtbl.find_opt ht (getTypeOfExp e) with
    | None -> false
    | Some a -> true) in if p || getTypeOfExp e cd = "Integer" || getTypeOfExp e cd = "String" then true else false
  | Aff(e1,e) -> getTypeOfExp e cd = getTypeOfExp e1 cd
  | Return -> true
  | Ite(e,i1,i2) -> getTypeOfExp e cd = "Integer" && checkInstr i1 ht cd && checkInstr i2 ht cd



let checkBlocPrincipal b cdl ht = if List.for_all (fun i -> List.length(List.filter (fun x -> checkInstr i ht x) cdl) > 0) b.instructions then true else false;;

let checkClass cd cdl = 
  if isFirstLetterUpper cd.name_class && verifExistOfClass cd.name_class cdl = false && verifExistOfSuperClass cd cdl then true else false

let checkClasses cdl =
  (* List.for_all (fun x -> checkClass x cdl)  cdl && verifyDoubleNameOfClass cdl  *)
true
(* let checkDefClasses cdl = 
   *)