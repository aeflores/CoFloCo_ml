(* header section *)
{
  open Lexing
  open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1
    }
    
}

(* definitions section *)

let capital = ['A'-'Z']         (* capital letters *)
let small = ['a'-'z']           (* small letters *)
let digit = ['0'-'9']           (* digits *)
let underline = ['_']           (* underline character *)

let alpha = capital | small | digit | underline          (* any alphanumeric character*)

let word = small alpha*                                  (* prolog words *)
let quoted_name = '\'' [^ '\'']+ '\''                     (* quoted names *)
let symbol = ['+' '-' '*' '/' '\\' '^' '<' '>' '=' '~' ':' '?' '@' '#' '$' '&'] 
let solo_char = ['!' ';' '.' '[' ']' '(' ')' ',' '|']               

let name = quoted_name | word | symbol+ | solo_char      (* valid prolog names *)

let variable = (capital | underline) alpha*              (* prolog variables *)


let sign = '+' | '-'                                     (* signs *)
let integer =  digit+           (* integers with no sign *)

let whitespace = [' ' '\t']

rule token = parse
        | eof {EOF}
        | '\n' {next_line lexbuf;token lexbuf}
        | whitespace     {token lexbuf}
	| "eq"     {EQUATION}
	| "input_output_vars"     {INPUT_OUTPUT}
	| "entry"     {ENTRY}
	| ">="     {GEQ}
        | "=<"     {LEQ}
        | "+"      {PLUS}
        | "-"      {MINUS}
	| "*"      {TIMES}
	|"("      {LPAREN}                (* left parenthesis *)
	| ")"      {RPAREN}                (* right parenthesis *)
	| ":"      {COLON}                 (* else *)
	| ","      {COMMA}                 (* logical and *)        
	| "="      {EQUAL}            (* unify terms *)
	| "<"      {LESS}            (* arithmetical less than *)
	| ">"      {GREATER}         (* arithmetical greater than *)
	| "["      {LBRACKET}              (* left bracket for lists *)
	| "]"      {RBRACKET}              (* right bracket for lists *)
        | "."      {DOT}
        | '%'      { single_line_comment lexbuf    }
        | "/*"     { multiline_comment 0 lexbuf    }
        | name as id          {NAME (id) }
        | integer 
        {  INTEGER (int_of_string (Lexing.lexeme lexbuf))   }        
        | variable               
        {VARIABLE (Lexing.lexeme lexbuf)}
	| _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
and single_line_comment = parse 
        | "\n" 
        {       token lexbuf    }
       
        | eof
        {       EOF       }

        |   _ 
        {       single_line_comment lexbuf       }

and multiline_comment level = parse
        | "*/"
        {       if level = 0 
                    then token lexbuf
                    else multiline_comment (level - 1) lexbuf    
        }
       
        | "/*"
        {       multiline_comment (level + 1) lexbuf    }

        | eof
        {       failwith "Unclosed comment!";           }

        |  _    
        {       multiline_comment level lexbuf          }

(* trailer section *)
{
}
