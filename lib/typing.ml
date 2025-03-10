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
        else if verif_declared_var vn env && (match i with IActOnNode (CreateAct, _, _) -> true | _ -> false)
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
  let r = tc_instrs_stop gt instrs in   (* call to real typechecker here *)
  match r with
  | Result.Error etc -> Printf.printf "%s\n" (String.concat "\n" etc); 
                        failwith "stopped"
  |_ -> np
  

  (* Typecheck program; 
     If continue is true, continue typechecking even 
     when errors have been discovered (can be ignored in a first version) *)  
     let typecheck continue (NormProg(gt, NormQuery instrs) as np) = 
      match check_graph_types gt with
      | Result.Error egt -> Printf.printf "%s\n" ("Types incorrects :\n" ^ (String.concat "\n" egt));
                            failwith "stopped"
      | Result.Ok _ -> typecheck_instructions continue gt instrs np
    

  
(*test de check_graph_types ou ya tout les erreurs:*)
let test_graph =  
        DBG (
          [
            DBN ("P", [("nom", Lang.StringT); ("age", Lang.IntT)]);
            DBN ("P", [("nom", Lang.StringT)]) (* Type dupliqué ici ! *)
          ],
          [
            DBR ("P", "ami", "P");
            DBR ("P", "emp", "E"); (* "E" non déclaré ! *)
            DBR ("P", "emp", "E")  (* Relation dupliquée ! *)
          ]
        );;

let test1 = check_graph_types test_graph;;

(*test de check_graph_types ou tout est bon *)
let test_types =  
 DBG ([( DBN ("P", [("nom", Lang. StringT ); ("age", Lang.IntT )]));
       (DBN ("E", [("nom", Lang. StringT ); ("pme", Lang. BoolT )]))] ,
   [( DBR ("P", "ami", "P"));
    (DBR ("P", "emp", "E"));
     (DBR ("E", "f", "E"))]) ;;

check_graph_types test_types ;;


(*Test de tc_instr *)
(* Définition d'un environnement de test *)
let env_test = { types = test_graph; bindings = [] };;

(* Test 1 : Création d'un nœud avec un label correct *)
let test_instr_create = IActOnNode (CreateAct, "x", "P");;
let result_create = tc_instr test_instr_create env_test;;

(* Test 2 : Match d'un nœud avec un label correct *)
let test_instr_match = IActOnNode (MatchAct, "y", "P");;
let result_match = tc_instr test_instr_match env_test;;

(* Test 3 : Création d'un nœud avec un label incorrect *)
let test_instr_invalid_label = IActOnNode (CreateAct, "z", "X");; (* "X" n'existe pas *)
let result_invalid_label = tc_instr test_instr_invalid_label env_test;;

(* Test 4 : Création d'une relation correcte entre deux nœuds déclarés *)
let env_with_vars = { types = test_graph; bindings = [("x", "P"); ("y", "P")] };;
let test_instr_rel = IActOnRel (CreateAct, "x", "ami", "y");;
let result_rel = tc_instr test_instr_rel env_with_vars;;

(* Test 5 : Création d'une relation avec une variable non déclarée *)
let test_instr_rel_invalid = IActOnRel (CreateAct, "x", "ami", "z");; (* "z" non déclaré *)
let result_rel_invalid = tc_instr test_instr_rel_invalid env_with_vars;;

(* Test 6 : Suppression d'un nœud déclaré *)
let test_instr_delete = IDeleteNode "x";;
let result_delete = tc_instr test_instr_delete env_with_vars;;

(* Test 7 : Suppression d'un nœud non déclaré *)
let test_instr_delete_invalid = IDeleteNode "z";; (* "z" non déclaré *)
let result_delete_invalid = tc_instr test_instr_delete_invalid env_with_vars;;



(*Test 8 : IReturn*)
let test_instr_return = IReturn ["x"; "y"];;
let result_return = tc_instr test_instr_return env_with_vars;;

let test_instr_return_invalid = IReturn ["x"; "z"];;
let result_return_invalid = tc_instr test_instr_return_invalid env_with_vars;;


(* norm prog : *)








