open Gmp;;
open Core.Std;;
open Utils;;
open Input_types;;

type var = Var of int

let print_var=function
    Var(n) -> print_string "V_";print_int n;;

module type number= sig
  type t
  val add: t->t->t
  val sub: t->t->t
  val mul: t->t->t
  val to_string: t->string
  val is_zero: t->bool
  val from_int: int->t
  val one: t
  val zero: t
  val sgn: t->int
end

module Linexp(N:number)= struct

  type  coeff= { var:var; value:N.t }
  type  t = {coeffs:coeff list; cnt:N.t} 
    
let get_coeff_val coeff=coeff.value

let is_constant {coeffs; cnt}=
  match coeffs with
  | [] -> true
  | x::xs-> false

let get_constant {coeffs;cnt}=cnt
let get_variables {coeffs;cnt}=
  List.map ~f:(fun {var;value} -> var) coeffs

let make_constant_from_int intcnt={coeffs=[];cnt=N.from_int(intcnt)}
let make_constant cnt={coeffs=[];cnt}
let make_variable var={coeffs=[{var;value=N.one}];cnt=N.from_int(0)}
let compare_coeff {var=var1;value=value1} {var=var2;value=value2}=
  if var1=var2 then 0
  else if var1>var2 then 1
  else -1

let rec applyop_coeffs op coeffs1 coeffs2=
  match coeffs1 with
  | [] -> coeffs2
  | {var=var1;value=c1}::tail1 -> 
    match coeffs2 with
    | [] -> {var=var1;value=c1}::tail1
    | {var=var2;value=c2}::tail2 ->
      if var1=var2 then 
	(let c3= op c1 c2 in
	 if (N.is_zero c3) then
	   applyop_coeffs op tail1 tail2
	 else
	   {var=var1;value=c3}:: applyop_coeffs op tail1 tail2
	)
      else if var1<var2 then
	{var=var1;value=c1}:: applyop_coeffs op tail1 ({var=var2;value=c2}::tail2)
      else
	{var=var2;value=c2}:: applyop_coeffs op ({var=var1;value=c1}::tail1) tail2

let rec applyop op {coeffs=coeffs1; cnt=cnt1} {coeffs=coeffs2; cnt=cnt2}=
     {coeffs=applyop_coeffs op coeffs1 coeffs2 ;cnt=op cnt1 cnt2}

let mulCoeff q {var;value}={var;value=N.mul q value}
let add l1 l2 = applyop N.add l1 l2
let mul {coeffs; cnt} q  = 
  {coeffs=List.map ~f:(mulCoeff q) coeffs;cnt=N.mul q cnt}
let negate l= mul l (N.from_int (0-1))
let sub l1 l2 = applyop N.add l1 (negate l2)

let is_positive_coeff {var;value}= (N.sgn value)=1

let get_dimension {coeffs; cnt}=
    (List.fold_left ~f:(fun x {var=Var(v);value} -> max x v) ~init:(0-1) coeffs)+1

let get_positive_negative {coeffs; cnt}=
  let (pos,neg)= List.partition_tf ~f:is_positive_coeff coeffs in
   if (N.sgn cnt)=1 then 
    ({coeffs=pos;cnt},negate {coeffs=neg;cnt=N.zero})
  else
    ({coeffs=pos;cnt=N.zero},negate {coeffs=neg;cnt})



let print_coeff {var;value}=
  if (value=N.one) then
    print_var var 
  else 
    (print_string (N.to_string value);
    print_string "*";
    print_var var)
 
    

let print l=
  let {coeffs;cnt}=l in
  match coeffs with 
    [] ->print_string (N.to_string cnt)
  | x::xs -> 
    print_coeff x;
    List.iter ~f:(fun {var;value} -> (if (N.sgn value)=1 then print_string "+");print_coeff {var;value}) xs;
    if not (N.is_zero cnt) then
      (print_string "+";
	print_string (N.to_string cnt))

(*
let numbervars_coeff (n,varmap) {var;value}=
 (match List.Assoc.find varmap var with
       | None -> (n+1, (List.Assoc.add varmap var (Var n)))
       | Some x -> (n, varmap)
    )
let numbervars (n,varmap) {coeffs;cnt}=
  List.fold_left ~f:numbervars_coeff ~init:(n,varmap) coeffs

let map_coeff varmap {var;value}=
  match List.Assoc.find varmap var with
       | None -> assert false
       | Some x -> {var=x;value}
    
let map  varmap {coeffs;cnt}=
  {coeffs=List.map ~f:(map_coeff varmap) coeffs;cnt}
*)

end;;

module Int_linexp = Linexp(Z);;

module Rat_linexp = Linexp(Q);;



let coeffq_to_coeff {Rat_linexp.var=var;Rat_linexp.value=value}= 
  {Int_linexp.var=var;
   Int_linexp.value=Q.get_num value}

let rat_linexpq_to_int_linexp {Rat_linexp.coeffs=coeffs;Rat_linexp.cnt=cnt}=
  {Int_linexp.coeffs=List.map ~f:coeffq_to_coeff coeffs;
   Int_linexp.cnt= Q.get_num cnt}

let normalizeLinexpq_with_den {Rat_linexp.coeffs=coeffs;Rat_linexp.cnt=cnt} =
  let values= List.map ~f:Rat_linexp.get_coeff_val coeffs in
  let denominators= List.map  ~f:Q.get_den values in
  let lcm=Q.from_z (List.fold_left ~f:Z.lcm ~init:(Q.get_den cnt) denominators) in
  ((Rat_linexp.mul {Rat_linexp.coeffs=coeffs;Rat_linexp.cnt=cnt} lcm) |> rat_linexpq_to_int_linexp, lcm)

let normalizeLinexpq l= 
  let l,den=normalizeLinexpq_with_den l in  l

let app_maybe op x=match x with
  | Some elem-> Some(op elem)
  | None -> None
let app_maybe_bin op x y =match (x,y) with
  | (Some elem,Some elem2)-> Some(op elem elem2)
  | _ -> None


  



let rec expression_to_lin_exp numvar exp=
  match exp with
  |  Atom(atom) -> (match atom with
                     | Integer(integer)-> Some(Rat_linexp.make_constant_from_int integer)
		     | Variable(name) -> 
		       Some(Rat_linexp.make_variable (Var (NumvarTbl.find_store numvar name)))
  )
  | Unary_Minus(exp2) -> app_maybe Rat_linexp.negate  (expression_to_lin_exp numvar exp2)
  | Plus(exp1,exp2) -> app_maybe_bin Rat_linexp.add (expression_to_lin_exp numvar exp1) (expression_to_lin_exp numvar exp2)
  | Minus(exp1,exp2) -> app_maybe_bin Rat_linexp.sub (expression_to_lin_exp numvar exp1) (expression_to_lin_exp numvar exp2)
  | Times(exp1,exp2) -> 
    (let option_lin_exp1=expression_to_lin_exp numvar exp1 in
    let option_lin_exp2=expression_to_lin_exp numvar exp2 in
      match option_lin_exp1 with
	Some lin_exp1 ->
	  (match option_lin_exp2 with
	    Some lin_exp2 -> 
	      (if (Rat_linexp.is_constant lin_exp1) then
		Some(Rat_linexp.mul lin_exp2 (Rat_linexp.get_constant lin_exp1))
	      else if  (Rat_linexp.is_constant lin_exp2) then
		Some(Rat_linexp.mul lin_exp1 (Rat_linexp.get_constant lin_exp2))
	      else
		None)
	  | None -> None
	  )
      | None -> None)
  | Div(exp1,exp2) -> 
    (let option_lin_exp1=expression_to_lin_exp numvar exp1 in
    let option_lin_exp2=expression_to_lin_exp numvar exp2 in
     match option_lin_exp1 with
	Some lin_exp1 ->
	  (match option_lin_exp2 with
	    Some lin_exp2 -> 
	       (if (Rat_linexp.is_constant lin_exp2) then
		Some(Rat_linexp.mul  lin_exp1 (Gmp.Q.inv (Rat_linexp.get_constant lin_exp2)))
	       else
		 None)
	  | None -> None)
     | None -> None)	  
  | Nat(exp) -> None
