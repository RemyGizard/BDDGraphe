(* Semantics of instructions *)
open Lang
open Graphstruct
open Instr


  (* State of the program, essentially a graph structure and a binding in form of a table,
  and as convenience info an overapproximation of the maximal node number
  allocated in the graph (could in principle be recomputed each time)
  Nodes have a unique identifier (an int) which is incremented when creating new nodes.
  When deleting nodes, the max node number is currently not decremented, 
  thus does not reflect the number of nodes in the graph, but one could think
  of a garbage collector when running out of node identifiers.
   *)

(* Naive implementation of bindings as tables, ie. 
   a heading (variable list 'v list) and a list of lines containing nodes 
   that each have the same length as the variable list  *)

type ('v, 'n) table = Table of 'v list * 'n list list
  [@@deriving show]

type vname_nodeid_table = (vname, nodeid) table
  [@@deriving show]

let empty_table = Table([], [[]])
(* Trouve l’index d’une variable dans la liste des en-têtes *)
let index_of x lst =
  let rec aux i = function
    | [] -> failwith ("Variable non trouvée dans la table : " ^ x)
    | y::ys -> if y = x then i else aux (i+1) ys
  in aux 0 lst


(* Add a single variable v, bound to a single node n, to a table,
  as during node creation (old interpretation, now superseded, 
  see create_node and add_var_mult_nodes_to_table) *)
let add_var_single_node_to_table v n (Table (vns, lns)) = 
    Table (v::vns, List.map (fun ln -> n::ln) lns)

(* Add multiple nodes contained in ns for a new variable v to a table,
  one node per line. ns and lns have to have the same length.  *)
let add_var_mult_nodes_to_table v ns (Table (vns, lns)) = 
      Table (v::vns, List.map2 (fun n ln -> n::ln) ns lns)
      


type attrib_val = fieldname * value
  [@@deriving show]
type attrib_struct = label * (attrib_val list)
  [@@deriving show]
      
type db_graph_struct = (Graphstruct.nodeid, attrib_struct, label) Graphstruct.db_graph
  [@@deriving show]
 

type state = State of db_graph_struct * (vname, nodeid) table * nodeid
let initial_state = State(empty_graph, empty_table, 0)

let create_node v lb (State(g, tab, mn)) = 
  let Table(_vns, lns) = tab in 
  let new_node_ids = List.init (List.length lns) (fun i -> mn +i) in 
  let new_nodes = List.init (List.length lns) (fun i -> DBN(mn + i , (lb, []))) in
  let new_tab = add_var_mult_nodes_to_table v new_node_ids tab in
  let new_graph = add_nodes_to_graph new_nodes g in 
  State (new_graph, new_tab, mn + 1)

  (* Crée une relation entre deux nœuds correspondant à deux variables *)
(* Crée une relation entre deux nœuds correspondant à deux variables *)
let create_rel src_var rel_label dst_var (State(g, Table(vns, lns), mn)) =
  let src_index = index_of src_var vns in
  let dst_index = index_of dst_var vns in

  (* DEBUG : Affiche le nom de la relation qu'on essaie de créer *)
  Printf.printf ">> Création relation : %s entre %s et %s\n" rel_label src_var dst_var;

  let new_rels = 
    List.map (fun line ->
      let src_id = List.nth line src_index in
      let dst_id = List.nth line dst_index in
      let r = DBR(src_id, rel_label, dst_id) in

      (* DEBUG : Affiche chaque relation construite *)
      Printf.printf "   Relation créée : %d -[:%s]-> %d\n" src_id rel_label dst_id;

      r
    ) lns
  in

  let new_graph = List.fold_left (fun acc rel -> add_rel_to_graph acc rel) g new_rels in
  State(new_graph, Table(vns, lns), mn)





(* TODO: complete following definition *)
let exec_instr s = function
  | IActOnNode (CreateAct, v, lb) ->
      create_node v lb s

  | IActOnRel (CreateAct, src_var, rel_label, dst_var) ->
      create_rel src_var rel_label dst_var s

  | _ -> s


let exec (NormProg(_tps, NormQuery(instrs))) = 
  List.fold_left exec_instr initial_state instrs
