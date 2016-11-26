open Core.Std;;



let main filename=
  Input.parse_eqs filename;
(*  Output.print_entries (); *)
(*   Output.print_crs (); *)
  let sccs=Scc.compute_sccs_and_cutsets () in
  Output.print_sccs sccs;
  Partial_evaluation.pe sccs
   
  


let spec =
  let open Command.Spec in
  empty
  +> anon ("filename" %: string)

let command =
  Command.basic
    ~summary:"Read cost equations"
    ~readme:(fun () -> "More detailed information")
    spec
    (fun filename () -> main filename)

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
