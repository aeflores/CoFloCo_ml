


%{
  open Input_types

%}


%token EQUATION
%token INPUT_OUTPUT
%token ENTRY
%token <string> VARIABLE
%token <string> NAME
%token <int> INTEGER
%token DOT 
%token NAT 

%token EQUAL LEQ GEQ GREATER LESS
%token PLUS MINUS TIMES

%token COMMA COLON
%token LPAREN RPAREN LBRACKET RBRACKET
%token EOF


%start <Input_types.clause list> crs

%%
crs: 
  xs=clause_list {xs};

clause_list:
   | EOF {[]}   
   | x=parse_clause; xs=clause_list;
    {x::xs}
;


parse_clause:	
  |  EQUATION; LPAREN; ihead=parse_head;COMMA;
     icost=parse_expression_nat;COMMA;icalls=parse_calls;COMMA;
     ics=parse_constraint_set; RPAREN; DOT
  { Equation({ihead;icost;icalls;ics})}
  |  INPUT_OUTPUT; LPAREN;ihead=parse_head;COMMA;
     input=parse_variable_list;COMMA;
     output=parse_variable_list; RPAREN;DOT
       { Io_vars({ihead;input;output})}
  |  ENTRY; LPAREN; ihead=parse_head; COLON;
     iprecondition=parse_constraint_set; RPAREN;DOT
       {Entry({ihead;iprecondition})}
;

parse_head: 
  iname=NAME; LPAREN; ivars=separated_list(COMMA,parse_atom); RPAREN 
    { {iname;ivars}}
  | iname=NAME;
    { {iname;ivars=[]}}
;

parse_calls:
  LBRACKET; calls=separated_list(COMMA,parse_call);RBRACKET
    {calls}
;
parse_call: 
  iname_call=NAME; LPAREN; ivars_call=parse_expression_list; RPAREN 
    { {iname_call;ivars_call}}
 | iname_call=NAME;
    { {iname_call;ivars_call=[]}}
;

parse_variable_list:
  LBRACKET; variable_list=separated_list(COMMA,parse_variable);RBRACKET
    {variable_list}
;
parse_constraint_set:
  LBRACKET; constraint_set=separated_list(COMMA,parse_constraint);RBRACKET
    {constraint_set}
;
parse_expression_list:
   expression_list=separated_list(COMMA,parse_expression);
    {expression_list}
;

parse_constraint:
    e1=parse_expression; rel=relational_op; e2=parse_expression
  { Constr(rel, e1, e2)};

parse_expression_nat:
  NAT;LPAREN; exp=parse_expression ;RPAREN { Nat(exp)}
  | exp=parse_expression { exp}
;
parse_expression:
  | e=parse_summand {e}
  |e1=parse_expression;PLUS; e2=parse_summand { Plus(e1,e2)}
  |e1=parse_expression;MINUS; e2=parse_summand {Minus(e1,e2)};

parse_summand:
  | e=parse_subexpression {e}
  | e1=parse_summand;TIMES; e2=parse_subexpression { Times(e1,e2)};

parse_subexpression:
  | atom=parse_atom {Atom(atom)}
  | MINUS; atom=parse_atom {Unary_Minus(Atom(atom))}
  | LPAREN; exp=parse_expression ; RPAREN {exp}
parse_atom: 
  | int=INTEGER {Integer int}
  | var=VARIABLE {Variable var};

   
parse_variable:
    var=VARIABLE {Variable var};

relational_op:
  | LESS {Less_Than}
  | LEQ  {Less_Or_Equal}
  | EQUAL {Equal}
  | GREATER {Greater_Than}
  | GEQ  {Greater_Or_Equal};
