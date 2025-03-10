open Graphstruct
open Lang
open Instr
 
type environment = { types:  db_tp; bindings: (vname * label) list }

let initial_environment gt = {types = gt; bindings = []}
let initial_result gt = Result.Ok (initial_environment gt)
  
exception FieldAccError of string
exception TypeError of string


type tc_result = (environment, string list) result

(* Functions for manipulating the environment *)

let add_var vn t (env:environment) = 
  {env with bindings = (vn,t)::env.bindings}

let remove_var vn env = 
  {env with bindings = List.remove_assoc vn env.bindings}

(* TODO: add more auxiliary functions here *)
let rec no_duplicates = function
  | [] -> true
  | (x :: xs) -> not (List.mem x xs) && (no_duplicates xs);;

let types_unique ntdecls = 
  no_duplicates (List.map (fun (DBN(n, _)) -> n) ntdecls) ;;


  
  
  let rec no_declared = function
  | [] -> true
  | (x :: xs) -> not (List.mem x xs) && no_declared xs

  let types_declared ntdecls rtdecls =
    let node_types = List.map (fun (DBN(n, _)) -> n) ntdecls in
    List.for_all (fun (DBR(src, _, dest)) -> List.mem src node_types && List.mem dest node_types) rtdecls
  


  let relations_unique rtdecls =
      no_duplicates (List.map (fun (DBR(src, label, dest)) -> (src, label, dest)) rtdecls)
    
  (* TODO: fill in details *)
  let check_graph_types (DBG (ntdecls, rtdecls)) = 
    let errors = 
      (if not (types_declared ntdecls rtdecls) then ["Types non déclarés"] else []) @
      (if not (types_unique ntdecls) then ["Types de nœuds dupliqués"] else []) @
      (if not (relations_unique rtdecls) then ["Relations dupliquées"] else [])
    in
    if errors = [] then Result.Ok () else Result.Error errors
  
  

(* TODO: fill in details *)
let rec tp_expr env = function
  Const v -> IntT
| AttribAcc (vn, fn) -> IntT
| BinOp (bop, e1, e2) -> tp_expr env e1

(* check expression e with expected type et in environment env *)
let check_expr e et env : tc_result = 
  try 
    if tp_expr env e = et
    then Result.Ok env
    else Result.Error ["Expression does not have expected type " ^ (show_attrib_tp et)]
  with 
  | TypeError s -> Result.Error [s]
  | FieldAccError s -> Result.Error [s]
  


(* Instructions *)
let verif_label lb env =
  match env.types with
  | DBG (ntdecls, _) -> List.mem lb (List.map (fun (DBN(n, _)) -> n) ntdecls);;


let verif_declared_var vn env =
  List.exists (fun (v, _) -> v = vn) env.bindings ;;

let add_var_to_env vn lb env =
  { env with bindings = (vn, lb) :: env.bindings } ;;

  let tc_instr (i: instruction) (env: environment) : tc_result = 
    match i with
    | IActOnNode (CreateAct, vn, lb) 
    | IActOnNode (MatchAct, vn, lb) -> 
        if not (verif_label lb env) 
        then Result.Error ["Label non déclaré"]
        else if verif_declared_var vn env && i = IActOnNode (CreateAct, vn, lb)
        then Result.Error ["Variable déjà déclarée"]
        else Result.Ok (add_var_to_env vn lb env)
  
    | IActOnRel (CreateAct, vn1, lb, vn2)
    | IActOnRel (MatchAct, vn1, lb, vn2) ->
        if not (verif_declared_var vn1 env) || not (verif_declared_var vn2 env)
        then Result.Error ["Variable source ou destination non déclarée"]
        else Result.Ok env
  
    | IDeleteNode vn ->
        if not (verif_declared_var vn env)
        then Result.Error ["Variable à supprimer non déclarée"]
        else Result.Ok { env with bindings = List.remove_assoc vn env.bindings }
  
    | IDeleteRel (vn1, lb, vn2) ->
        if not (verif_declared_var vn1 env) || not (verif_declared_var vn2 env)
        then Result.Error ["Variable source ou destination non déclarée"]
        else Result.Ok env
  
    | IReturn vnames ->
        if List.exists (fun vn -> not (verif_declared_var vn env)) vnames
        then Result.Error ["Variable inconnue dans return"]
        else if no_duplicates vnames
        then Result.Ok { env with bindings = List.filter (fun (v, _) -> List.mem v vnames) env.bindings }
        else Result.Error ["Variables dupliquées dans return"]
  
    | IWhere expr ->
        (match check_expr expr BoolT env with
         | Result.Ok _ -> Result.Ok env
         | Result.Error e -> Result.Error e)
  
    | ISet (vn, attr, expr) ->
        if not (verif_declared_var vn env)
        then Result.Error ["Variable inconnue dans set"]
        else Result.Ok env
  
    | _  -> Result.Error ["Instruction non implémentée"]
   ;;

        

(* type check list of instructions and stop on error *)
let check_and_stop (res : tc_result) i : tc_result = Result.bind res (tc_instr i)

let tc_instrs_stop gt instrs : tc_result = 
  List.fold_left check_and_stop (initial_result gt) instrs


  (* TODO: typecheck instructions *)
let typecheck_instructions continue gt instrs np = 
  let r = Result.Ok initial_environment in   (* call to real typechecker here *)
  match r with
  | Result.Error etc -> Printf.printf "%s\n" (String.concat "\n" etc); 
                        failwith "stopped"
  |_ -> np
  

  (* Typecheck program; 
     If continue is true, continue typechecking even 
     when errors have been discovered (can be ignored in a first version) *)  
let typecheck continue (NormProg(gt, NormQuery instrs) as np) = 
  match check_graph_types gt with
  | Result.Error egt -> Printf.printf "%s\n" ("Undeclared types in\n" ^ egt);
                        failwith "stopped"
  | _ -> typecheck_instructions continue gt instrs np
  

  let test_duplicate_relations =  
    DBG (
      [
        DBN ("P", [("nom", Lang.StringT); ("age", Lang.IntT)]);
        DBN ("E", [("nom", Lang.StringT); ("pme", Lang.BoolT)])
      ],
      [
        DBR ("P", "ami", "P");
        DBR ("P", "emp", "E");
        DBR ("P", "emp", "E"); 
        DBR ("E", "f", "E")
      ]
    );;
  check_graph_types test_duplicate_relations;;



  

  

