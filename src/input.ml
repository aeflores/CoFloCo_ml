
open Core.Std;;
open Input_types;;
open Linear_expression;;
open Constraint_set;;
open Utils;;
open Db;;
open Lexer;;
open Lexing;;

let atom_to_var numvar accum_constr=function
     Integer(int) ->  
       (let new_var=NumvarTbl.fresh numvar in
       let eq_constr=Constr.Eq(Int_linexp.sub (Int_linexp.make_variable (Var new_var)) (Int_linexp.make_constant_from_int int)) in
       (Var(new_var),eq_constr::accum_constr))
  |Variable(name) -> (Var(NumvarTbl.find_store numvar name),accum_constr)

let expression_to_var numvar accum_constr =function
  | Atom(Variable(name))-> (Var(NumvarTbl.find_store numvar name),accum_constr)
  | expr -> 
       let new_var=NumvarTbl.fresh numvar in
	match (expression_to_lin_exp numvar expr) with
	  Some(linexp) -> 
	    let rat_linexp=Rat_linexp.sub (Rat_linexp.make_variable (Var new_var)) linexp in
	    let int_linexp=normalizeLinexpq rat_linexp  in
	    let eq_constr=Constr.Eq(int_linexp) in
	    (Var(new_var),eq_constr::accum_constr)
        | None -> (Var(new_var),accum_constr)
  



let rec list_to_vars f varmap accum_constr=function
   atom::atoms -> 
     let (var,accum_constr1)=f varmap accum_constr atom in
     let (vars,accum_constr2)=list_to_vars f varmap accum_constr1 atoms in
      (var::vars,accum_constr2)
  | [] -> ([], accum_constr)


let ihead_to_head varmap accum_constr {iname=nm;ivars=vs}=
  let (varlist,accum_constr1)=list_to_vars atom_to_var varmap accum_constr vs in
   ({name=nm;vars=varlist},accum_constr1)

let icall_to_head varmap accum_constr {iname_call=nm;ivars_call=vs}=
  let (varlist,accum_constr1)=list_to_vars expression_to_var varmap accum_constr vs in
   ({name=nm;vars=varlist},accum_constr1)

let rec icalls_to_heads varmap accum_constr =function
   call::calls -> let (head,accum_constr1)=icall_to_head varmap accum_constr call in
		  let (heads,accum_constrs2)=icalls_to_heads varmap accum_constr1 calls in
		  (head::heads,accum_constrs2)
  | [] -> ([], accum_constr)

let iconstraint_set_to_constraint_set numvar cs =
  let iconstr_to_constr_warning accum constr=
    match (Constr.iconstr_to_constr numvar constr) with
      Some(constr) -> constr::accum
    | None -> 
      (print_string "Ignored non-linear constraint:";
      (*iexpression_print constr;*)
      accum)
   in
  List.fold_left ~f:iconstr_to_constr_warning  ~init:[] cs

let ientry_to_entry {ihead;iprecondition=ics}=
  let numvar=NumvarTbl.create () in
  let (ehead,extra_constrs)=ihead_to_head numvar [] ihead in
  let cs= iconstraint_set_to_constraint_set numvar ics in
  {ehead; precondition=(extra_constrs @cs);text_names=numvar}

let expression_to_cost_structure numvar icost=
  match (expression_to_lin_exp numvar icost) with
    Some(exp) -> exp
  |  None -> 
         Rat_linexp.make_variable (Var( NumvarTbl.fresh numvar))

let iequation_to_equation {ihead;icost;icalls;ics}=
  let numvar=NumvarTbl.create () in
  let (head,accum_constr)=ihead_to_head numvar [] ihead in
  let (calls,accum_constr1)=icalls_to_heads numvar accum_constr icalls in
  let cost= expression_to_cost_structure numvar icost in
  let cs1= iconstraint_set_to_constraint_set numvar ics in
  {head;cost;calls;cs=accum_constr1 @cs1;text_names=numvar}


let io_vars_to_empty_cost_relation  {ihead;input;output}=
    let numvar=NumvarTbl.create () in
    let (head,_)=ihead_to_head numvar [] ihead in
    let (ivars,_)=list_to_vars atom_to_var numvar [] input in
    let (ovars,_)=list_to_vars atom_to_var numvar [] output in
    {head;ivars;ovars;eqs=[]}




let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
let parse_with_error lexbuf =
  try Parser.crs Lexer.token lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    []
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)


let split_clauses clauses=
  let split_clause (eqs,entries,ios) clause=match clause with 
    | Equation eq -> (eq::eqs,entries,ios)
    | Io_vars io -> (eqs,entries,io::ios)
    | Entry entry -> (eqs,entry::entries,ios) in
  let (eqs,entries,ios)=List.fold_left ~f:split_clause ~init:([],[],[]) clauses in
  (List.rev eqs, entries,ios)
  


let store_entry entry=Hashtbl.set Db.entries entry.ehead.name  entry 

let store_eq (eq:equation)=match (Hashtbl.find Db.crs eq.head.name) with
      None -> Hashtbl.set Db.crs eq.head.name 
	{head=eq.head;ivars=eq.head.vars;ovars=[];eqs=[eq]}
    | Some({head;ivars;ovars;eqs}) -> 
      Hashtbl.set Db.crs eq.head.name {head;ivars;ovars;eqs=eq::eqs}

let store_io empty_cr=match (Hashtbl.find Db.crs empty_cr.head.name) with
        None -> Hashtbl.set Db.crs empty_cr.head.name empty_cr
    | Some(cr) -> print_string " repeated input output vars\n";;

(* printf  "%a: repeated input output vars\n" empty_cr.head.name;; *)

let  parse_and_store lexbuf =
  let (ieqs,ientries,io_info)= (parse_with_error lexbuf |> split_clauses) in 
  let eqs=List.map ~f:iequation_to_equation ieqs in
  let entries=List.map ~f:ientry_to_entry ientries in
  let empty_cr=List.map ~f:io_vars_to_empty_cost_relation io_info in
   List.iter ~f:store_io empty_cr;
   (match entries with 
    [] -> 
      (match eqs with
	[] -> assert false
      | first_eq::_ -> 
	store_entry {ehead=first_eq.head;precondition=[];text_names=first_eq.text_names}
      )
   | e::es-> List.iter ~f:store_entry entries);
   List.iter ~f:store_eq eqs

let feasible_equation eq= Ppl.ppl_consistent eq.cs
let remove_undefined_calls {head;cost;calls;cs;text_names}=
  let (calls2,undefined)=List.partition_tf ~f:(fun call -> Hashtbl.mem Db.crs call.name) calls in
  if not (List.is_empty undefined) then
    (print_string "%Removed undefined calls in " ;
    Output.print_term head;
    print_string "to : " ;
    Output.print_separated_list Output.print_term "," undefined;
    print_string "\n " 
    );
  {head;cost;calls=calls2;cs;text_names}

let remove_unecessary_variables {head;cost;calls;cs;text_names}=
  let vars=List.concat (List.map ~f:(fun x -> x.vars) (head::calls)) in
  let vars2=Rat_linexp.get_variables cost in
  let var_set=List.dedup (vars @ vars2) in
  let cs2=Ppl.ppl_project_change_dimension var_set cs in
   {head;cost;calls;cs=cs2;text_names}
(*fixme: text_names should be updated*)

let normalize_cr name=
  match Hashtbl.find Db.crs name with
    Some {head;ivars;ovars;eqs}->
       let eqs3=(List.map ~f:remove_undefined_calls eqs 
             |> List.map ~f:remove_unecessary_variables) in
       let (feasible,discarded)=List.partition_tf ~f:feasible_equation eqs3 in
       List.iter ~f:(fun eq ->print_string "%Discarded:";Output.print_ce eq) discarded;
       Hashtbl.replace Db.crs name {head;ivars;ovars;eqs=feasible}
  | None -> assert false
let normalize_crs ()=
  List.iter ~f:normalize_cr (Hashtbl.keys Db.crs)


let parse_eqs filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_store lexbuf;
  In_channel.close inx;
  normalize_crs ()
 
