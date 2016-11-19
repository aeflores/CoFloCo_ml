open Core.Std
(*
type mytype = (int, int) Map.Poly.t
let mapa=Map.Poly.empty
let mapa1=Map.Poly.add mapa 2 3  
let rec read_and_accumulate accum =
  let line = In_channel.input_line In_channel.stdin in 
  match line with
  | None -> accum
  | Some x -> read_and_accumulate (accum +. Float.of_string x)
*)

let () =
  let res=  Ppl.ppl_project [Ppl.v1;Ppl.v2;Ppl.v5] [Ppl.c1;Ppl.c2;Ppl.c3] in
  match List.map ~f:Ppl.print_constraint res with
    _ -> ();;

