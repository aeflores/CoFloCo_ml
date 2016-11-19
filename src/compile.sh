
ocamlfind ocamlopt -o utils.opt -c  -package gmp -package core -thread  utils.ml
ocamlfind ocamlopt -o input_types.opt -c  -package gmp -package core -thread  input_types.ml

menhir parser.mly --infer
ocamlfind ocamlopt -o parser.cmi -c  parser.mli
ocamlfind ocamlopt -o parser.opt -c  parser.ml
ocamlopt -c parser.mli

ocamllex lexer.mll
ocamlfind ocamlopt -o lexer.opt  lexer.ml

ocamlfind ocamlopt -o linear_expression.opt -c  -package gmp -package core -thread  linear_expression.ml
ocamlfind ocamlopt -o constraint_set.opt -c  -package gmp -package core -thread  constraint_set.ml

ocamlfind ocamlopt -o db.opt -c  -package gmp -package core -thread  db.ml

ocamlfind ocamlopt -o ppl.opt  -c -I /usr/local/lib/ppl/  -package gmp -package core -thread  ppl.ml

ocamlfind ocamlopt -o lexer.opt -c  -package gmp -package core -thread  lexer.ml

ocamlfind ocamlopt -o output.opt -c  -package gmp -package core -thread  output.ml
ocamlfind ocamlopt -o input.opt -c  -package gmp -package core -thread  input.ml

ocamlfind ocamlopt -o input.native -cc g++ -I /home/aeflores/.opam/system/lib/gmp -I /usr/local/lib/ppl   -cclib -lppl -cclib -lm -cclib -lgmpxx -cclib -lgmp   -linkpkg -package core -thread ppl_ocaml.cmxa gmp.cmxa utils.cmx input_types.cmx parser.cmx lexer.cmx linear_expression.cmx constraint_set.cmx db.cmx ppl.cmx output.cmx input.cmx

#ocamlfind ocamlopt -o ppl.opt  -c -I /usr/local/lib/ppl/  -package gmp -package core -thread  ppl.ml

#ocamlfind ocamlopt -o main.opt  -c -I /usr/local/lib/ppl/  -package gmp -package core -thread  main.ml

#ocamlfind ocamlopt -o main.native -cc g++ -I /home/aeflores/.opam/system/lib/gmp -I /usr/local/lib/ppl   -cclib -lppl -cclib -lm -cclib -lgmpxx -cclib -lgmp   -linkpkg -package core -thread  ppl_ocaml.cmxa gmp.cmxa ppl.cmx main.cmx
