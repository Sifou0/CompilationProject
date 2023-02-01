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
  | x::s -> if (String.equal x.className str) then true else (verifExistOfClass str s)
;;

(* Permet de vérifier que la super classe existe *)
let rec verifExistOfSuperClass x l =
  match x.superclass with
  | None -> true
  | Some s -> verifExistOfSuperClass s l
;;


let verifyInHeritance l =
  List.fold_left (
    fun acc x -> acc && (
      if (verifExistOfSuperClass x l) then true
      else raise (VC_Error ("La super classe de \"" ^ x.name_class ^ "\" n'est pas définie.")) )) true l
;;

let rec countNameClass str l =
  match l with
  | [] -> 0
  | x::s -> if (String.equal x.name_class str) then (countNameClass str s) + 1 else (countNameClass str s)
;;

let verifyDoubleNameOfClass l =
  List.fold_left (fun acc x -> acc && (
    if ((countNameClas x.name_class l) < 2) then true else raise (VC_Error ("La classe \"" ^ x.name_class ^ "\" est définie plusieurs fois.")))) true l
;;


let getOptString s =
  match s with
  | None -> ""
  | Some str -> str
;;

(* vérifier si un mot clé "str" est utilisé dans l'expression *)
let rec verifyExistanceOfExpKey str e =
  match e with
  | Id s -> String.equal str s
  | Cste _ -> false
  | Plus(g,d) -> (verifyExistanceOfExpKey str g) || (verifyExistanceOfExpKey str d)
  | Minus(g,d) -> (verifyExistanceOfExpKey str g) || (verifyExistanceOfExpKey str d)
  | Times(g,d) -> (verifyExistanceOfExpKey str g) || (verifyExistanceOfExpKey str d)
  | Div(g,d) -> (verifyExistanceOfExpKey str g) || (verifyExistanceOfExpKey str d)

  | Comp(_,g,d) -> (verifyExistanceOfExpKey str g) || (verifyExistanceOfExpKey str d)
  | Cast(s,e) -> (String.equal str s) || (verifyExistanceOfExpKey str e)
  | NewInstance(s,le) -> (String.equal str s) || (List.fold_left (fun acc x -> acc && (verifyExistanceOfExpKey str x)) false le)
  | StringCste s -> String.equal str s
  | Concate(g,d) -> (verifyExistanceOfExpKey str g) || (verifyExistanceOfExpKey str d)


  let rec verifyExistanceOfInstrKey str i =
    match i with
    | Exp e -> verifyExistanceOfExpKey str e;
    | Aff(e1,e2) -> (verifyExistanceOfExpKey str e1) || ( verifyExistanceOfExpKey str e2)

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
  let rec extractOverride ld =
    match ld with
    | [] -> []
    | (MethodDecl m)::s -> if (m.is_override) then m::(extractOverride s) else extractOverride s
    | _::s -> extractOverride s
  in extractOverride c.block_class
;;

(* Compare si deux listes de declaration sont équivalentes *)
let rec compareDecls ld1 ld2 =
  match ld1 with
  | [] ->
    begin
      match ld2 with
      | [] -> true
      | _ -> false
    end
  | d1::s1 ->
    begin
      match ld2 with
      | [] -> false
      | d2::s2 -> if ((List.length d1.lhs) == (List.length d2.lhs) && (String.equal d1.rhs d2.rhs)) then compareDecls s1 s2 else false
    end
;;

(* vérifie si la signature de la méthode "m" fait partie de la liste des methodes "lm" *)
let rec verifyExistanceMeth m lm =
  match lm with
  | [] -> false
  | meth::l -> if (
  (String.equal meth.methodName m.methodName) && 
  (String.equal (getOptString meth.return_type) (getOptString m.return_type)) && 
  (compareDecls meth.params m.args)) then true else  verifyExistanceMeth m l
;;

(* vérifie que les méthodes définie comme "override" sont bien en train d'override une méthode parent *)
let verifyUsageOverride ld =
  let rec verifOverride l =
    match l with
    | [] -> true
    | c::s -> let lm = getMethodesRedefind c ld
              in let lmo = getOverrideMethods c
              in (List.fold_left (fun acc m -> acc && (
                if (vverifyExistanceMeth m lm) then true
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
let rec verifyYesConstructor ld =
  match ld with
  | [] -> true
  | c::s -> let nbConstructor = List.fold_left (fun acc x -> match x with | Constructor _  -> acc+1 | _ -> acc) 0 c.classBody
    in if (nbConstructor == 0) then
      raise (VC_Error ("La classe \"" ^ c.className ^ "\" n'a pas de constructeur."))
    else if (nbConstructor > 1) then
      raise (VC_Error ("La classe \"" ^ c.className ^ "\" a plusieurs constructeurs."))
    else
      verifyYesConstructor s
;;

let getConstrFromClass c =
  c.constructor
;;

(* Compare si deux listes de paramètres sont équivalentes *)
let rec compareLenParam lp1 lp2 =
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
;;

(* Récupère le nom de la classe d'une instanciation ainsi que ces paramètres *)
let getInstanciationInfo i =
  match i with
  | NewInstance(str,l) -> (str,l)
  | _ -> raise (VC_Error ("getInstanciationInfo : argument incorrect."))
;;

(* Retourne le nombre de paramètres nécessaires pour instancier la classe "c" *)
let getNbParamsRequiered c =
  let rec gtnbparam acc lp =
    match lp with
    |[] ->acc
    |_::s  -> gtnbparam (acc+1) s
  in
  gtnbparam 0 c.params
;;

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






