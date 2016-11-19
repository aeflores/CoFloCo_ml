open Core.Std;;
open Linear_expression;;
open Constraint_set;;
open Utils;;
open Db;;

let print_separated_list print_elem separator list=
    match list with
      [] -> ()
    | x::xs ->
       print_elem x;
       List.iter ~f:(fun v ->print_string separator;print_elem v) xs

let print_term  {name;vars} =
  printf "%s(" name;
  print_separated_list print_var "," vars;
  printf ")"

let print_constraint_set cs =
  printf "[" ;
  print_separated_list Constr.print "," cs;
  printf "]"

let print_entry entry=
  let {ehead;precondition;text_names}=entry in 
  print_string "entry(";
  print_term ehead; 
  print_string ":";
  print_constraint_set precondition;
  print_string ").\n"
 

let print_ce {head;cost;calls;cs;text_names}=
  print_string "eq(";
  print_term head;
  print_string ",";
  Rat_linexp.print cost;
  print_string ",[";
  print_separated_list print_term "," calls;
  print_string "],";
  print_constraint_set cs;
  print_string ").\n"



let print_cr {head;ivars;ovars;eqs} =
  print_string "\ninput_output_vars(";
  print_term head;
  print_string ",["; print_separated_list print_var "," ivars;
  print_string "],["; print_separated_list print_var "," ovars;
  print_string "]).\n";
  List.iter ~f:print_ce eqs;
  print_string "\n"

let print_entries ()=
  print_string "%Entries:\n";
  Hashtbl.iter ~f:(fun ~key:key ~data:value-> print_entry value) Db.entries

let print_crs ()=
  print_string "%CRS:\n";
  Hashtbl.iter ~f:(fun ~key:key ~data:value-> print_cr value) Db.crs
