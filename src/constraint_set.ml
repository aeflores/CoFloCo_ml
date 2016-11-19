open Linear_expression;;
open Input_types;;
open Core.Std;;


module Constr= struct
  type t=
  | Geq of Int_linexp.t
  | Eq of Int_linexp.t;;



let iconstr_to_constr numvar constr  =
  let Constr(rel_op,e1,e2)=constr in
  let option_linexp1= expression_to_lin_exp numvar e1 in
  let option_linexp2= expression_to_lin_exp numvar e2 in
  match (option_linexp1,option_linexp2) with
    (Some(linexp1),Some(linexp2))-> 
      let constr=(
	match rel_op with
	  Less_Than-> 
	    let linexp1p=Rat_linexp.add linexp1 (Rat_linexp.make_constant_from_int 1) in
	    Geq(normalizeLinexpq (Rat_linexp.sub linexp2 linexp1p))
	| Less_Or_Equal->
	  Geq(normalizeLinexpq (Rat_linexp.sub linexp2 linexp1))
	| Equal-> 
          Eq(normalizeLinexpq (Rat_linexp.sub linexp1 linexp2))
	| Greater_Than->
	  let linexp2p=Rat_linexp.add linexp2 (Rat_linexp.make_constant_from_int 1) in
          Geq(normalizeLinexpq (Rat_linexp.sub linexp1 linexp2p))
	| Greater_Or_Equal->
          Geq(normalizeLinexpq (Rat_linexp.sub linexp1 linexp2))) in
      Some(constr)
  | _ -> None

let get_dimension =function
  | Geq(linexp) ->Int_linexp.get_dimension linexp
  | Eq(linexp) ->   Int_linexp.get_dimension linexp
let print= function
  | Geq(linexp) ->
    let (pos,neg)=Int_linexp.get_positive_negative linexp in
    Int_linexp.print pos;print_string ">=";Int_linexp.print neg
  | Eq(linexp) ->   
    let (pos,neg)=Int_linexp.get_positive_negative linexp in
    Int_linexp.print pos;print_string "=";Int_linexp.print neg

end;;


let cs_get_dimension cs = 
  List.fold_left ~f:(fun dim constr -> max dim (Constr.get_dimension constr)) ~init:0  cs
