
open Core.Std;;
open Ppl_ocaml;;
open Linear_expression;;
open Constraint_set;;
open Utils;;
open Gmp;;

let coeff_to_pplLinexp {Int_linexp.var=var; Int_linexp.value=value}=
  let Var(varnum)=var in
    Times(value,Variable varnum)

let linexp_to_pplLinexp {Int_linexp.coeffs=coeffs;Int_linexp.cnt=cnt}=
  let coeffs1=List.map ~f:coeff_to_pplLinexp coeffs in
  let cnt1= Coefficient(cnt) in
  List.fold_left ~f:(fun exp elem ->Plus(exp,elem)) ~init:cnt1 coeffs1
 
let constr_to_pplConstr= function
  |Constr.Geq(linexp) -> Greater_Or_Equal(linexp_to_pplLinexp linexp,Coefficient Z.zero)
  | Constr.Eq(linexp) -> Equal(linexp_to_pplLinexp linexp,Coefficient Z.zero)

let rec pplLinexp_to_linexp= function
   Variable v ->Int_linexp.make_variable (Var v)
  | Coefficient c ->Int_linexp.make_constant c
  | Unary_Minus e ->Int_linexp.negate (pplLinexp_to_linexp  e)
  | Unary_Plus e ->pplLinexp_to_linexp  e
  | Plus (e1, e2) ->
    Int_linexp.add (pplLinexp_to_linexp  e1) (pplLinexp_to_linexp  e2)
  | Minus (e1, e2) ->
    Int_linexp.sub (pplLinexp_to_linexp  e1) (pplLinexp_to_linexp  e2)
  | Times (c, e) ->
    Int_linexp.mul  (pplLinexp_to_linexp  e) c
let pplConstr_to_constr= function
   Less_Or_Equal (le1, le2) ->
     let linexp1= pplLinexp_to_linexp  le1 in
     let linexp2= pplLinexp_to_linexp  le2 in
     Constr.Geq (Int_linexp.sub linexp2 linexp1)
  | Equal (le1, le2) ->
     let linexp1= pplLinexp_to_linexp  le1 in
     let linexp2= pplLinexp_to_linexp  le2 in
     Constr.Eq (Int_linexp.sub linexp2 linexp1)
  | Greater_Or_Equal (le1, le2) ->
     let linexp1= pplLinexp_to_linexp  le1 in
     let linexp2= pplLinexp_to_linexp  le2 in
     Constr.Geq (Int_linexp.sub linexp1 linexp2)
  | Less_Than (le1, le2) ->
    assert false
  | Greater_Than (le1, le2) ->
    assert false


let polyhedron_to_pplPolyhedron cs=
  List.map ~f:constr_to_pplConstr cs
let pplPolyhedron_to_polyhedron cs=
  List.map ~f:pplConstr_to_constr cs


let numbervar_coeff mapvar {Int_linexp.var=Var(var);Int_linexp.value=value}=
  let new_var=Var (NumvarTbl_int.find_store mapvar var) in
  {Int_linexp.var=new_var;Int_linexp.value=value}
    
let numbervar_linexp mapvar {Int_linexp.coeffs=coeffs;Int_linexp.cnt=cnt}=
  let coeffs1=List.map ~f:(numbervar_coeff mapvar) coeffs in
  let sorted_coeffs= List.sort ~cmp:Int_linexp.compare_coeff coeffs1 in
  {Int_linexp.coeffs=sorted_coeffs;Int_linexp.cnt=cnt}

let numbervar_constr mapvar =function
  Constr.Eq e -> Constr.Eq (numbervar_linexp mapvar e)
  | Constr.Geq e->Constr.Geq  (numbervar_linexp mapvar e)

let numbervar_polyhedron mapvar cs=
  List.map ~f:(numbervar_constr mapvar) cs

let numbervar_varlist mapvar vars=
  List.map ~f:(fun (Var var)-> Var (NumvarTbl_int.find_store mapvar var)) vars




let from_ppl p = 
  Ppl_ocaml.ppl_Polyhedron_get_constraints p |>
  pplPolyhedron_to_polyhedron
 


let from_ppl_minimized p=
  Ppl_ocaml.ppl_Polyhedron_get_minimized_constraints p |>
  pplPolyhedron_to_polyhedron



let to_ppl_dimension dim cs=
  let p=ppl_new_C_Polyhedron_from_space_dimension dim Universe  in
   ppl_Polyhedron_add_constraints p (polyhedron_to_pplPolyhedron cs);
   p

 
let ppl_get_points p= 
     ppl_Polyhedron_get_minimized_generators p 
     |> List.filter ~f:(function  Point (_,_)-> true | _ -> false) 

 
let ppl_project vars cs=
  let numvar=NumvarTbl_int.create () in
  let _=numbervar_varlist numvar vars in
  let n_important=NumvarTbl_int.get_length numvar in
  let cs2=numbervar_polyhedron numvar cs in
  let n_total=NumvarTbl_int.get_length numvar in
  let p=to_ppl_dimension n_total cs2 in
     ppl_Polyhedron_remove_higher_space_dimensions p n_important;
     from_ppl p |>
     numbervar_polyhedron (NumvarTbl_int.reverse numvar)
 
let ppl_project_change_dimension vars cs=
  let numvar=NumvarTbl_int.create () in
  let _=numbervar_varlist numvar vars in
  let n_important=NumvarTbl_int.get_length numvar in
  let cs2=numbervar_polyhedron numvar cs in
  let n_total=NumvarTbl_int.get_length numvar in
  let p=to_ppl_dimension n_total cs2 in
     ppl_Polyhedron_remove_higher_space_dimensions p n_important;
     from_ppl p

let binary_ppl_op op cs1 cs2=
   let numvar=NumvarTbl_int.create () in
   let cs1_mapped=numbervar_polyhedron numvar cs1 in
   let cs2_mapped=numbervar_polyhedron numvar cs2 in
   let n=NumvarTbl_int.get_length numvar in
   let p1= to_ppl_dimension n cs1_mapped in
   let p2= to_ppl_dimension n cs2_mapped in
       op p1 p2;
       from_ppl p1 |>  numbervar_polyhedron (NumvarTbl_int.reverse numvar)

let binary_ppl_op_direct op cs1 cs2=
  let n= (max (cs_get_dimension cs1) (cs_get_dimension cs2)) in
  let p1= to_ppl_dimension n cs1 in
  let p2= to_ppl_dimension n cs2 in
       op p1 p2; from_ppl p1

let ppl_lub cs1 cs2=binary_ppl_op ppl_Polyhedron_upper_bound_assign cs1 cs2 
let ppl_glb cs1 cs2=binary_ppl_op ppl_Polyhedron_intersection_assign cs1 cs2 
let ppl_widen cs1 cs2=binary_ppl_op ppl_Polyhedron_H79_widening_assign cs1 cs2 


let ppl_entails cs1 cs2=
  let numvar=NumvarTbl_int.create () in
   let cs1_mapped=numbervar_polyhedron numvar cs1 in
   let cs2_mapped=numbervar_polyhedron numvar cs2 in
   let n=NumvarTbl_int.get_length numvar in
   let p1= to_ppl_dimension n cs1_mapped in
   let p2= to_ppl_dimension n cs2_mapped in
   ppl_Polyhedron_contains_Polyhedron p1 p2


let ppl_consistent cs=
   let numvar=NumvarTbl_int.create () in
   let cs_mapped=numbervar_polyhedron numvar cs in
   let n=NumvarTbl_int.get_length numvar in
   let p= to_ppl_dimension n cs_mapped in
     not (ppl_Polyhedron_is_empty p)

(*
let ppl_all_ranking_functions vars cs =
  let numvar=NumvarTbl_int.create () in
  let _=numbervar_varlist numvar vars in
  let n_important=NumvarTbl_int.get_length numvar in
  let cs2=numbervar_polyhedron numvar cs in
  let n_total=NumvarTbl_int.get_length numvar in
  let p=to_ppl_dimension n_total cs2 in
  let rf_p=ppl_all_affine_ranking_functions_PR_C_Polyhedron p in
  ppl_Polyhedron_remove_higher_space_dimensions rf_p n_important;
  ppl_get_points rf_p |> map_generators varmap2
*)
 
