open Core.Std;;

module NumvarTbl= struct
   type t={mutable length:int; table: int String.Table.t}
   let create () ={length=0; table=String.Table.create () ~size:5}

   let fresh_name int=
     Printf.sprintf "fresh_%d" int
   let fresh numvar=
     let res= numvar.length in 
	(Hashtbl.set numvar.table (fresh_name res) res;
	numvar.length <- numvar.length+1;
	res)

   let find_store numvar elem=
     match (Hashtbl.find numvar.table elem) with
        Some num-> num
      | None ->   
	let res= numvar.length in 
	Hashtbl.set numvar.table elem numvar.length;
	numvar.length <- numvar.length+1;
	res
 
end;;


module NumvarTbl_int= struct
   type t={mutable length:int; table: int Int.Table.t}
   let create () ={length=0; table=Int.Table.create () ~size:5}
   let get_length {length;table}=length

   let find_store numvar elem=
     match (Hashtbl.find numvar.table elem) with
        Some num-> num
      | None ->   
	let res= numvar.length in 
	Hashtbl.set numvar.table elem numvar.length;
	numvar.length <- numvar.length+1;
	res

   let reverse {length;table}=
     let new_table=Int.Table.create () ~size:length in
     Hashtbl.iter ~f:(fun ~key:key ~data:data -> Hashtbl.set new_table data key) table;
     {length;table=new_table}
 
end;;
