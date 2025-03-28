open Graphstruct
open Lang
open Instr

(* === Types et Environnement === *)
type environment = { types:  db_tp; bindings: (vname * label) list }
let initial_environment gt = {types = gt; bindings = []}
let initial_result gt = Result.Ok (initial_environment gt)

exception FieldAccError of string
exception TypeError of string

type tc_result = (environment, string list) result

(* === Fonctions auxiliaires === *)
let add_var vn t (env:environment) = {env with bindings = (vn,t)::env.bindings}
let remove_var vn env = {env with bindings = List.remove_assoc vn env.bindings}

let rec no_duplicates = function
  | [] -> true
  | x::xs -> not (List.mem x xs) && no_duplicates xs

let types_unique ntdecls =
  no_duplicates (List.map (fun (DBN(n, _)) -> n) ntdecls)

let relations_unique rtdecls =
  no_duplicates (List.map (fun (DBR(src, label, dest)) -> (src, label, dest)) rtdecls)

let types_declared ntdecls rtdecls =
  let node_types = List.map (fun (DBN(n, _)) -> n) ntdecls in
  List.for_all (fun (DBR(src, _, dest)) -> List.mem src node_types && List.mem dest node_types) rtdecls

let check_graph_types (DBG (ntdecls, rtdecls)) =
  let errors =
    (if not (types_declared ntdecls rtdecls) then ["Types non déclarés"] else []) @
    (if not (types_unique ntdecls) then ["Types de nœuds dupliqués"] else []) @
    (if not (relations_unique rtdecls) then ["Relations dupliquées"] else [])
  in
  if errors = [] then Result.Ok () else Result.Error errors

(* === Typage des expressions === *)
let rec tp_expr env = function
  | Const (IntV _) -> IntT
  | Const (BoolV _) -> BoolT
  | Const (StringV _) -> StringT
  | AttribAcc (vn, fn) ->
    (match List.assoc_opt vn env.bindings with
     | None -> raise (FieldAccError ("Variable " ^ vn ^ " non déclarée"))
     | Some label ->
       match env.types with
       | DBG (ntdecls, _) ->
         let rec find = function
           | [] -> raise (FieldAccError ("Label " ^ label ^ " non trouvé"))
           | DBN(n, attrs)::rest ->
             if n = label then
               match List.assoc_opt fn attrs with
               | Some t -> t
               | None -> raise (FieldAccError ("Champ " ^ fn ^ " non trouvé dans " ^ label))
             else find rest
         in find ntdecls)
  | BinOp (bop, e1, e2) ->
    let t1 = tp_expr env e1 in
    let t2 = tp_expr env e2 in
    match bop with
    | BArith op ->
      (match op with
       | BAadd | BAsub | BAmul | BAdiv | BAmod ->
         if t1 = IntT && t2 = IntT then IntT
         else raise (TypeError "Erreur : opération arithmétique mal typée"))
    | BCompar op ->
      (match op with
       | BCeq | BCge | BCgt | BCle | BClt | BCne ->
         if t1 = IntT && t2 = IntT then BoolT
         else raise (TypeError "Erreur : comparaison mal typée"))
    | BLogic op ->
      (match op with
       | BLand | BLor ->
         if t1 = BoolT && t2 = BoolT then BoolT
         else raise (TypeError "Erreur : opération logique mal typée"))

let check_expr e et env : tc_result =
  try
    if tp_expr env e = et then Result.Ok env
    else Result.Error ["Expression does not have expected type " ^ (show_attrib_tp et)]
  with
  | TypeError s -> Result.Error [s]
  | FieldAccError s -> Result.Error [s]

(* === Vérification des instructions === *)
let verif_label lb env =
  match env.types with
  | DBG (ntdecls, _) -> List.mem lb (List.map (fun (DBN(n, _)) -> n) ntdecls)

let verif_declared_var vn env =
  List.exists (fun (v, _) -> v = vn) env.bindings

let add_var_to_env vn lb env =
  { env with bindings = (vn, lb) :: env.bindings }

let tc_instr (i: instruction) (env: environment) : tc_result =
  match i with
  | IActOnNode (CreateAct, vn, lb)
  | IActOnNode (MatchAct, vn, lb) ->
    if not (verif_label lb env)
    then Result.Error ["Label non déclaré"]
    else if verif_declared_var vn env && (match i with IActOnNode (CreateAct, _, _) -> true | _ -> false)
    then Result.Error ["Variable déjà déclarée"]
    else Result.Ok (add_var_to_env vn lb env)

  | IActOnRel (_, vn1, _, vn2)
  | IDeleteRel (vn1, _, vn2) ->
    if not (verif_declared_var vn1 env) || not (verif_declared_var vn2 env)
    then Result.Error ["Variable source ou destination non déclarée"]
    else Result.Ok env

  | IDeleteNode vn ->
    if not (verif_declared_var vn env)
    then Result.Error ["Variable à supprimer non déclarée"]
    else Result.Ok (remove_var vn env)

  | IReturn vnames ->
    if List.exists (fun vn -> not (verif_declared_var vn env)) vnames
    then Result.Error ["Variable inconnue dans return"]
    else if no_duplicates vnames
    then Result.Ok { env with bindings = List.filter (fun (v, _) -> List.mem v vnames) env.bindings }
    else Result.Error ["Variables dupliquées dans return"]

  | IWhere expr -> check_expr expr BoolT env

  | ISet (vn, attr, expr) ->
    if not (verif_declared_var vn env)
    then Result.Error ["Variable inconnue dans set"]
    else
      match List.assoc_opt vn env.bindings with
      | None -> Result.Error ["Variable inconnue (bindings incohérents)"]
      | Some label ->
        match env.types with
        | DBG (ntdecls, _) ->
          match List.assoc_opt label (List.map (fun (DBN(n, attrs)) -> (n, attrs)) ntdecls) with
          | None -> Result.Error ["Label "^label^" non trouvé"]
          | Some attrs ->
            (match List.assoc_opt attr attrs with
             | None -> Result.Error ["Attribut "^attr^" non trouvé"]
             | Some atype ->
               try
                 if tp_expr env expr = atype then Result.Ok env
                 else Result.Error ["Type incorrect pour l'attribut "^attr]
               with
               | TypeError s | FieldAccError s -> Result.Error [s])

(* === Vérification de programme complet === *)
let check_and_stop (res : tc_result) i : tc_result =
  Result.bind res (tc_instr i)

let tc_instrs_stop gt instrs : tc_result =
  List.fold_left check_and_stop (initial_result gt) instrs

let typecheck_instructions continue gt instrs np =
  let r = tc_instrs_stop gt instrs in
  match r with
  | Result.Error etc ->
    Printf.printf "%s\n" (String.concat "\n" etc);
    failwith "stopped"
  | _ -> np

let typecheck continue (NormProg(gt, NormQuery instrs) as np) =
  match check_graph_types gt with
  | Result.Error egt ->
    Printf.printf "%s\n" ("Types incorrects :\n" ^ (String.concat "\n" egt));
    failwith "stopped"
  | Result.Ok _ ->
    typecheck_instructions continue gt instrs np

(* === Tests === *)
let print_tc_result msg r =
  Printf.printf "\n%s\n" msg;
  match r with
  | Result.Ok _ -> Printf.printf " OK\n"
  | Result.Error errs -> List.iter (fun e -> Printf.printf " %s\n" e) errs

let test_types = DBG (
  [ DBN ("P", [("nom", StringT); ("age", IntT)]);
    DBN ("E", [("nom", StringT); ("pme", BoolT)]) ],
  [ DBR ("P", "ami", "P"); DBR ("P", "emp", "E"); DBR ("E", "f", "E") ])

let env0 = { types = test_types; bindings = [] }
let env2 = { types = test_types; bindings = [("x", "P"); ("y", "P")] }

let test_1 () = print_tc_result "Test 1 - Création de noeud valide" (tc_instr (IActOnNode (CreateAct, "x", "P")) env0)
let test_2 () = print_tc_result "Test 2 - Match d'un noeud valide" (tc_instr (IActOnNode (MatchAct, "y", "P")) env0)
let test_3 () = print_tc_result "Test 3 - Label inexistant" (tc_instr (IActOnNode (CreateAct, "z", "X")) env0)
let test_4 () = print_tc_result "Test 4 - Relation correcte" (tc_instr (IActOnRel (CreateAct, "x", "ami", "y")) env2)
let test_5 () = print_tc_result "Test 5 - Relation avec variable inconnue" (tc_instr (IActOnRel (CreateAct, "x", "ami", "z")) env2)
let test_6 () = print_tc_result "Test 6 - Suppression d’un noeud existant" (tc_instr (IDeleteNode "x") env2)
let test_7 () = print_tc_result "Test 7 - Suppression d’un noeud non déclaré" (tc_instr (IDeleteNode "z") env2)
let test_8 () = print_tc_result "Test 8 - Return valide" (tc_instr (IReturn ["x"; "y"]) env2)
let test_9 () = print_tc_result "Test 9 - Return avec variable inconnue" (tc_instr (IReturn ["x"; "z"]) env2)
let test_10 () = print_tc_result "Test 10 - Set valide" (tc_instr (ISet ("x", "age", Const (IntV 30))) env2)
let test_11 () = print_tc_result "Test 11 - Set invalide (mauvais type)" (tc_instr (ISet ("x", "age", Const (StringV "trente"))) env2)
let test_12 () = print_tc_result "Test 12 - WHERE correcte" (tc_instr (IWhere (BinOp (BCompar BCeq, AttribAcc ("x", "age"), Const (IntV 5)))) env2)
let test_13 () = print_tc_result "Test 13 - WHERE incorrecte" (tc_instr (IWhere (BinOp (BLogic BLand, Const (BoolV true), Const (IntV 1)))) env2)

(* === Test complet d’un programme normalisé === *)
let test_norm () =
  let prog = NormProg(test_types, NormQuery [
    IActOnNode (CreateAct, "a", "P");
    IActOnNode (CreateAct, "b", "E");
    IActOnRel (CreateAct, "a", "emp", "b");
    IReturn ["a"]
  ]) in
  let _ = typecheck false prog in
  Printf.printf "\nTest NormProg :  OK\n"

let test_norm_invalid () =
  let prog = NormProg(test_types, NormQuery [
    IActOnNode (CreateAct, "a", "P");
    IActOnRel (CreateAct, "a", "emp", "z");
  ]) in
  try ignore (typecheck false prog)
  with Failure _ -> Printf.printf "\nTest NormProg invalide :  Erreur attendue\n"

  let run_tests () =
    test_1 (); test_2 (); test_3 (); test_4 (); test_5 ();
    test_6 (); test_7 (); test_8 (); test_9 (); test_10 ();
    test_11 (); test_12 (); test_13 (); test_norm (); test_norm_invalid ()
  

