open Core.Std;;
open Scc;;

let has_multiple_elems =function
    [] -> false
  | [one] -> false
  | one::two::xs -> true

let pe_scc {entries;nodes; recursive; cutset}=
  if (has_multiple_elems nodes) then
    ()
 
let pe sccs=
  List.iter ~f:pe_scc sccs
