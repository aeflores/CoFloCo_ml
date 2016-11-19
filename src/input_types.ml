type name = string       (* prolog names *)

type atom =
| Integer of int
| Variable of string
type arithmetic_operator =
    Plus                          (* term1 + term2 *)            
  | Minus                         (* term1 - term2 *)       
  | Mult                          (* term1 * term2 *)      
  | Div                           (* term1 / term2 *)    
type relational_operator=
    Less                          (* term1 < term2 *)      
  | Greater                       (* term1 > term2 *)         
  | Leq                           (* term1 <= term2 *)     
  | Geq                           (* term1 >= term2 *)     
  | Equal                               (* term1 = term2 *) 

type expression =
    Atom of atom
  | Unary_Minus of expression
  | Plus of expression * expression
  | Minus of expression * expression
  | Times of expression * expression
  | Div of expression * expression
  | Nat of expression

type rel_op=
    Less_Than
  | Less_Or_Equal
  | Equal
  | Greater_Than
  | Greater_Or_Equal

type iconstr = Constr of rel_op * expression * expression
 


type iconstraint_set = iconstr list
		    		      
type ihead = {iname:name;ivars: atom list}
type icall = {iname_call:name;ivars_call: expression list}
type iequation={ihead:ihead;icost:expression;icalls:icall list;ics:iconstraint_set}
type io_vars={ihead:ihead;input:atom list;output:atom list}
type ientry={ihead:ihead;iprecondition:iconstraint_set}
 
type clause =
   Equation of iequation
| Io_vars of io_vars
| Entry of ientry
