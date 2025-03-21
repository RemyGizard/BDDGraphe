open Graphstruct
open Lang
open Instr

type environment = { types:  db_tp; bindings: (vname * label) list }

let initial_environment gt = {types = gt; bindings = []}
let initial_result gt = Result.Ok (initial_environment gt)

exception FieldAccError of string
exception TypeError of string

type tc_result = (environment, string list) result

(* Fonctions auxiliaires *)

let add_var vn t (env:environment) =
  {env with bindings = (vn,t)::env.bindings}

let remove_var vn env =
  {env with bindings = List.remove_assoc vn env.bindings}

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

(* Typage des expressions *)

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

(* Instructions *)

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

  | IActOnRel (CreateAct, vn1, _, vn2)
  | IActOnRel (MatchAct, vn1, _, vn2) ->
      if not (verif_declared_var vn1 env) || not (verif_declared_var vn2 env)
      then Result.Error ["Variable source ou destination non déclarée"]
      else Result.Ok env

  | IDeleteNode vn ->
      if not (verif_declared_var vn env)
      then Result.Error ["Variable à supprimer non déclarée"]
      else Result.Ok (remove_var vn env)

  | IDeleteRel (vn1, _, vn2) ->
      if not (verif_declared_var vn1 env) || not (verif_declared_var vn2 env)
      then Result.Error ["Variable source ou destination non déclarée"]
      else Result.Ok env

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



(* Instructions multiples *)

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



      (* ======================== *)
(*         TESTS           *)
(* ======================== *)

(* Test de check_graph_types avec erreurs *)
let test_graph =
  DBG (
    [
      DBN ("P", [("nom", StringT); ("age", IntT)]);
      DBN ("P", [("nom", StringT)]) (* Type dupliqué ici ! *)
    ],
    [
      DBR ("P", "ami", "P");
      DBR ("P", "emp", "E"); (* "E" non déclaré ! *)
      DBR ("P", "emp", "E")  (* Relation dupliquée ! *)
    ]
  )

let test1 = check_graph_types test_graph

(* Test de check_graph_types sans erreur *)
let test_types =
  DBG ([
      DBN ("P", [("nom", StringT); ("age", IntT)]);
      DBN ("E", [("nom", StringT); ("pme", BoolT)])
    ],
    [
      DBR ("P", "ami", "P");
      DBR ("P", "emp", "E");
      DBR ("E", "f", "E")
    ])

let _ = check_graph_types test_types

(* Environnement de test vide *)
let env_test = { types = test_types; bindings = [] }

(* Test : création d’un nœud correct *)
let test_instr_create = IActOnNode (CreateAct, "x", "P")
let result_create = tc_instr test_instr_create env_test

(* Test : match d’un nœud avec un label correct *)
let test_instr_match = IActOnNode (MatchAct, "y", "P")
let result_match = tc_instr test_instr_match env_test

(* Test : label inexistant *)
let test_instr_invalid_label = IActOnNode (CreateAct, "z", "X")
let result_invalid_label = tc_instr test_instr_invalid_label env_test

(* Environnement avec deux variables déjà déclarées *)
let env_with_vars = { types = test_types; bindings = [("x", "P"); ("y", "P")] }

(* Test : relation correcte *)
let test_instr_rel = IActOnRel (CreateAct, "x", "ami", "y")
let result_rel = tc_instr test_instr_rel env_with_vars

(* Test : relation avec variable inconnue *)
let test_instr_rel_invalid = IActOnRel (CreateAct, "x", "ami", "z")
let result_rel_invalid = tc_instr test_instr_rel_invalid env_with_vars

(* Test : suppression d’un nœud déclaré *)
let test_instr_delete = IDeleteNode "x"
let result_delete = tc_instr test_instr_delete env_with_vars

(* Test : suppression d’un nœud non déclaré *)
let test_instr_delete_invalid = IDeleteNode "z"
let result_delete_invalid = tc_instr test_instr_delete_invalid env_with_vars

(* Test : instruction RETURN correcte *)
let test_instr_return = IReturn ["x"; "y"]
let result_return = tc_instr test_instr_return env_with_vars

(* Test : instruction RETURN avec variable inconnue *)
let test_instr_return_invalid = IReturn ["x"; "z"]
let result_return_invalid = tc_instr test_instr_return_invalid env_with_vars

(* Test : ISet correct *)
let test_instr_set = ISet ("x", "age", Const (IntV 30))
let result_set = tc_instr test_instr_set env_with_vars

(* Test : ISet avec mauvais type *)
let test_instr_set_err = ISet ("x", "age", Const (StringV "trente"))
let result_set_err = tc_instr test_instr_set_err env_with_vars

(* Test : IWhere avec expression correcte *)
let expr_bool = BinOp (BCompar BCeq, Const (IntV 1), Const (IntV 1))
let test_instr_where = IWhere expr_bool
let result_where = tc_instr test_instr_where env_with_vars

(* Test : expression mal typée dans IWhere *)
let expr_err = BinOp (BLogic BLand, Const (BoolV true), Const (IntV 1))
let test_instr_where_err = IWhere expr_err
let result_where_err = tc_instr test_instr_where_err env_with_vars

(* Test simple de tp_expr *)
let _ =
  let e = BinOp (BArith BAadd, Const (IntV 2), Const (IntV 3)) in
  let ty = tp_expr env_test e in
  Printf.printf "Type de e : %s\n" (show_attrib_tp ty)








