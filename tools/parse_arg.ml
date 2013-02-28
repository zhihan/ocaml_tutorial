(* Parse the input arguments to figure out ocamlroot and proverlib *)
let get_opt (arg_list: string list) = 
  let ocamlroot = ref "" in
  let proverlib = ref "" in
  let args = Array.of_list arg_list in
  let i = ref 0 in
  while !i < Array.length args do
    match (Array.get args !i) with
      | "-ocamlroot" ->
          begin
            i := !i + 1 ;
            ocamlroot := Array.get args !i  
          end
      | "-proverlib" ->
          begin
            i := !i + 1 ;
            proverlib := Array.get args !i
          end
      | _ -> ()
          ;
          i := !i +1  
  done;
  (!ocamlroot, !proverlib) ;;
