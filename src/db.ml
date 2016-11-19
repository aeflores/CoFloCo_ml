
open Core.Std;;
open Linear_expression;;
open Constraint_set;;
open Input_types;;
open Utils;;

type term= {name:name;vars: var list}
type entry={ehead:term;precondition:Constr.t list;text_names:NumvarTbl.t}
type equation={head:term;cost:Rat_linexp.t;calls:term list;cs:Constr.t list;text_names:NumvarTbl.t}


type cost_relation={head:term;ivars:var list;ovars:var list;eqs: equation list}


let entries: entry String.Table.t = String.Table.create  () ~size:5;;
let crs:cost_relation String.Table.t = String.Table.create  () ~size:20;;




 
