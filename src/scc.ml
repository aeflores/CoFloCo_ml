open Core.Std;;
open Db;;
open Graph;;



module G = struct
  module String = struct
    type t = string
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = (=)
    let default = 0
  end
  include Imperative.Digraph.ConcreteBidirectional(String)
end

module Scc_components = Components.Make(G);;
module MinCutSet = Mincut.Make(G);;
module StringSet = Set.Make(String);;

type scc={entries: string list;nodes: string list; recursive:bool; cutset: string}

exception Irreducible_SCC of string

let add_ce_edges g {head;cost;calls;cs;text_names}=
  List.iter ~f:(fun call -> G.add_edge g head.name call.name) calls

let add_cr_edges g {head;ivars;ovars;eqs}=
  List.iter ~f:(add_ce_edges g) eqs

let cofloco_entry="CoFloCo_ENTRY";;
let cofloco_scc_entry="CoFloCo_SCC_ENTRY";;
  
let create_callgraph () =
  let g= G.create () in
  Hashtbl.iter ~f:(fun ~key:key ~data:data ->add_cr_edges g data) crs;
  Hashtbl.iter ~f:(fun ~key:key ~data:data ->G.add_edge g cofloco_entry key) entries;
  g

let is_scc_recursive g=function
    [] -> assert false
  | [x] -> G.mem_edge g x x
  |  x1::x2::xs -> true

let is_entry g node_set node =
  let preds=StringSet.of_list (G.pred g node) in
  let diff=StringSet.diff preds node_set in
   not (StringSet.is_empty diff)

let compute_scc_entries g nodes=
  let node_set=StringSet.of_list nodes in
    List.filter ~f:(is_entry g node_set)  nodes 
   

let add_edges_from g2 g node=
    List.iter ~f:(fun dest -> G.add_edge g2 node dest) (G.succ g node)

let get_scc_restricted_graph g entries nodes=
  let g2= G.create () in
  List.iter ~f:(fun node -> add_edges_from g2 g node) nodes;
  List.iter ~f:(fun entry -> G.add_edge g2 cofloco_scc_entry entry) entries;
  g2
   
 
let compute_scc_cutset g entries nodes=
  let g_restricted=get_scc_restricted_graph g entries nodes in
  match (MinCutSet.min_cutset g_restricted cofloco_scc_entry) with
    [] ->assert false
  | [one] -> one
  | one::two::rest ->raise (Irreducible_SCC "Multiple cutset") 
 
     

let compute_annotated_scc g nodes =
  let recursive= is_scc_recursive g nodes in
  let entries=compute_scc_entries g nodes in
  if recursive then
    let cutset= compute_scc_cutset g entries nodes in 
    {entries;nodes; recursive;cutset}
  else
    match nodes with
     [node] -> {entries;nodes; recursive;cutset=node}
    | _ -> assert false
	 



let compute_sccs_and_cutsets ()=
  let g=create_callgraph () in
  let basic_sccs=Scc_components.scc_list g in
  List.map ~f:(compute_annotated_scc g) basic_sccs



  
